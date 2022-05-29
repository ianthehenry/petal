// the "tokenizer" actually does a little bit of what a traditional parser would
// handle, treating parenthesized expressions as individual "tokens" (for
// example). the main difference between the tokenizer and the parser is the
// resolution of semicolons into parenthesized or bracketed expressions

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, digit1, multispace1, one_of},
    combinator::{map, map_res, opt, success},
    multi::{many1, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult, Parser,
};

#[derive(Debug, PartialEq)]
pub enum OldToken {
    Int64(i64),
    Parens(Vec<OldToken>),
    Brackets(Vec<OldToken>),
    Identifier(String),
    Semicolons(usize),
}

// TODO: obviously this is terrible. also it might make sense to wait to convert
// this into an actual number until later in the pipeline. also we might need to
// special-case rational numbers at some point huh.
fn number(i: &str) -> IResult<&str, OldToken> {
    let (i, sign): (_, i64) = alt((char('-').map(|_| -1), success(1)))(i)?;
    map_res(digit1, move |digit_str: &str| {
        (digit_str.parse::<i64>()).map(|num| OldToken::Int64(sign * num))
    })(i)
}

fn identifier(i: &str) -> IResult<&str, OldToken> {
    map(alpha1, |sym_str: &str| {
        OldToken::Identifier(sym_str.to_string())
    })(i)
}

fn brackets(i: &str) -> IResult<&str, OldToken> {
    map(delimited(char('['), tokens, char(']')), |tokens| {
        OldToken::Brackets(tokens)
    })(i)
}

fn parens(i: &str) -> IResult<&str, OldToken> {
    map(delimited(char('('), tokens, char(')')), |tokens| {
        OldToken::Parens(tokens)
    })(i)
}

fn unary_negation(i: &str) -> IResult<&str, OldToken> {
    // TODO: probably this should be some kind of builtin
    map(preceded(char('-'), token), |token| {
        OldToken::Parens(vec![OldToken::Identifier("neg".to_string()), token])
    })(i)
}

fn operator(i: &str) -> IResult<&str, OldToken> {
    map(one_of("+-*."), |op| OldToken::Identifier(op.to_string()))(i)
}

fn semicolons(i: &str) -> IResult<&str, OldToken> {
    map(many1(char(';')), |semis| OldToken::Semicolons(semis.len()))(i)
}

fn token(i: &str) -> IResult<&str, OldToken> {
    alt((
        number,
        unary_negation,
        identifier,
        operator,
        parens,
        brackets,
        semicolons,
    ))(i)
}

fn flatten_tokens(tokens: Vec<Vec<OldToken>>) -> Vec<OldToken> {
    tokens.into_iter().flatten().collect()
}

fn compound_token(i: &str) -> IResult<&str, Vec<OldToken>> {
    map(
        many1(tuple((token, opt(tag("-")))).map(|(token, subtraction)| {
            if let Some(op) = subtraction {
                vec![token, OldToken::Identifier(op.to_string())]
            } else {
                vec![token]
            }
        })),
        flatten_tokens,
    )(i)
}

fn tokens(i: &str) -> IResult<&str, Vec<OldToken>> {
    map(separated_list0(multispace1, compound_token), flatten_tokens)(i)
}

pub fn tokenize(i: &str) -> Vec<OldToken> {
    let (remaining, tokens) = tokens(i).unwrap();
    if !remaining.is_empty() {
        panic!("tokenize error");
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    impl OldToken {
        fn delimited(start: &str, tokens: Vec<OldToken>, end: &str) -> String {
            let mut result = start.to_string();
            result += &tokens
                .into_iter()
                .map(|token| token.to_short_string())
                .collect::<Vec<_>>()
                .join(" ");
            result += end;
            result
        }

        fn to_short_string(self) -> String {
            match self {
                OldToken::Int64(num) => num.to_string(),
                OldToken::Identifier(id) => id.to_string(),
                OldToken::Semicolons(count) => ";".repeat(count),
                OldToken::Parens(tokens) => OldToken::delimited("(", tokens, ")"),
                OldToken::Brackets(tokens) => OldToken::delimited("[", tokens, "]"),
            }
        }
    }

    fn test(input: &str) -> String {
        let (remaining, parsed) = tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        OldToken::delimited("", parsed, "")
    }

    #[test]
    fn number_literal() {
        k9::snapshot!(test("9223372036854775807"), "9223372036854775807");
        k9::snapshot!(test("-9223372036854775807"), "-9223372036854775807");
        // TODO: this should parse as a bigint literal
        k9::snapshot!(
            tokens("9223372036854775808"),
            r#"Ok(("9223372036854775808", []))"#
        );
        // TODO: this should parse as a numeric literal
        k9::snapshot!(
            tokens("-9223372036854775808"),
            r#"Ok(("9223372036854775808", [Identifier("-")]))"#
        );
    }

    #[test]
    fn hyphens() {
        k9::snapshot!(test("-x"), "(neg x)");
        k9::snapshot!(test("--x"), "(neg (neg x))");
        k9::snapshot!(test("- x"), "- x");
        k9::snapshot!(test("- -x"), "- (neg x)");
        k9::snapshot!(test("x-y"), "x - y");
        k9::snapshot!(test("-x-y"), "(neg x) - y");
        k9::snapshot!(test("x--y"), "x - (neg y)");
        k9::snapshot!(test("-x--y"), "(neg x) - (neg y)");
        k9::snapshot!(test("--x--y"), "(neg (neg x)) - (neg y)");
        k9::snapshot!(test("x*-y"), "x * - y");
        k9::snapshot!(test("x-*y"), "x - * y");
        k9::snapshot!(test("x* -y"), "x * (neg y)");
        k9::snapshot!(test("x*(-y)"), "x * ((neg y))");
        k9::snapshot!(test("-(x)"), "(neg (x))");
    }

    #[test]
    fn semicolons() {
        k9::snapshot!(test("[x;y]"), "[x ; y]");
        k9::snapshot!(test("[x;;y]"), "[x ;; y]");
        k9::snapshot!(test("[x;y;;z]"), "[x ; y ;; z]");
        k9::snapshot!(test("[x;; ; ;;;y]"), "[x ;; ; ;;; y]");
    }
}
