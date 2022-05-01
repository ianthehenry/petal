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
pub enum Token {
    Int64(i64),
    Parens(Vec<Token>),
    Brackets(Vec<Token>),
    Identifier(String),
    Semicolons(usize),
}

// TODO: obviously this is terrible. also it might make sense to wait to convert
// this into an actual number until later in the pipeline. also we might need to
// special-case rational numbers at some point huh.
fn number(i: &str) -> IResult<&str, Token> {
    let (i, sign): (_, i64) = alt((char('-').map(|_| -1), success(1)))(i)?;
    map_res(digit1, move |digit_str: &str| {
        (digit_str.parse::<i64>()).map(|num| Token::Int64(sign * num))
    })(i)
}

fn identifier(i: &str) -> IResult<&str, Token> {
    map(alpha1, |sym_str: &str| {
        Token::Identifier(sym_str.to_string())
    })(i)
}

fn brackets(i: &str) -> IResult<&str, Token> {
    map(delimited(char('['), tokens, char(']')), |tokens| {
        Token::Brackets(tokens)
    })(i)
}

fn parens(i: &str) -> IResult<&str, Token> {
    map(delimited(char('('), tokens, char(')')), |tokens| {
        Token::Parens(tokens)
    })(i)
}

fn unary_negation(i: &str) -> IResult<&str, Token> {
    // TODO: probably this should be some kind of builtin
    map(preceded(char('-'), token), |token| {
        Token::Parens(vec![Token::Identifier("neg".to_string()), token])
    })(i)
}

fn operator(i: &str) -> IResult<&str, Token> {
    map(one_of("+-*."), |op| Token::Identifier(op.to_string()))(i)
}

fn semicolons(i: &str) -> IResult<&str, Token> {
    map(many1(char(';')), |semis| Token::Semicolons(semis.len()))(i)
}

fn token(i: &str) -> IResult<&str, Token> {
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

fn flatten_tokens(tokens: Vec<Vec<Token>>) -> Vec<Token> {
    tokens.into_iter().flatten().collect()
}

fn compound_token(i: &str) -> IResult<&str, Vec<Token>> {
    map(
        many1(tuple((token, opt(tag("-")))).map(|(token, subtraction)| {
            if let Some(op) = subtraction {
                vec![token, Token::Identifier(op.to_string())]
            } else {
                vec![token]
            }
        })),
        flatten_tokens,
    )(i)
}

pub fn tokens(i: &str) -> IResult<&str, Vec<Token>> {
    map(separated_list0(multispace1, compound_token), flatten_tokens)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Token {
        fn delimited(start: &str, tokens: Vec<Token>, end: &str) -> String {
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
                Token::Int64(num) => num.to_string(),
                Token::Identifier(id) => id.to_string(),
                Token::Semicolons(count) => ";".repeat(count),
                Token::Parens(tokens) => Token::delimited("(", tokens, ")"),
                Token::Brackets(tokens) => Token::delimited("[", tokens, "]"),
            }
        }
    }

    fn test(input: &str) -> String {
        let (remaining, parsed) = tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        Token::delimited("", parsed, "")
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
    }

    #[test]
    fn semicolons() {
        k9::snapshot!(test("[x;y]"), "[x ; y]");
        k9::snapshot!(test("[x;;y]"), "[x ;; y]");
        k9::snapshot!(test("[x;y;;z]"), "[x ; y ;; z]");
        k9::snapshot!(test("[x;; ; ;;;y]"), "[x ;; ; ;;; y]");
    }
}
