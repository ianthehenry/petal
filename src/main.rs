use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{alpha1, char, digit1, multispace0, multispace1, one_of},
    combinator::{cut, map, map_res, opt, success},
    error::{context, VerboseError},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

#[derive(Debug, PartialEq)]
pub enum Word {
    Int64(i64),
    Parens(Vec<Word>),
    Identifier(String),
}

impl Word {
    fn to_short_string(&self) -> String {
        match self {
            Word::Int64(num) => num.to_string(),
            Word::Identifier(id) => id.to_string(),
            Word::Parens(words) => {
                ({
                    let mut result = "(".to_string();
                    result += &words
                        .iter()
                        .map(|word| word.to_short_string())
                        .collect::<Vec<_>>()
                        .join(" ");
                    result += ")";
                    result
                })
            }
        }
    }
}

// TODO: obviously this is terrible
fn parse_num<'a>(i: &'a str) -> IResult<&'a str, Word> {
    let (i, sign): (_, i64) = alt((char('-').map(|_| -1), success(1)))(i)?;
    let x = map_res(digit1, |digit_str: &str| {
        (digit_str.parse::<i64>()).map(|num| Word::Int64(sign * num))
    })(i);
    x
}

fn parse_identifier<'a>(i: &'a str) -> IResult<&'a str, Word> {
    map(alpha1, |sym_str: &str| {
        Word::Identifier(sym_str.to_string())
    })(i)
}

fn parse_parens<'a>(i: &'a str) -> IResult<&'a str, Word> {
    map(delimited(char('('), parse_words, char(')')), |words| {
        Word::Parens(words)
    })(i)
}

fn parse_unary_negation<'a>(i: &'a str) -> IResult<&'a str, Word> {
    // TODO: probably this should be some kind of builtin
    map(preceded(char('-'), parse_word), |word| {
        Word::Parens(vec![Word::Identifier("neg".to_string()), word])
    })(i)
}

fn parse_operator<'a>(i: &'a str) -> IResult<&'a str, Word> {
    map(one_of("+-*"), |op| Word::Identifier(op.to_string()))(i)
}

fn parse_word<'a>(i: &'a str) -> IResult<&'a str, Word> {
    alt((
        parse_unary_negation,
        parse_num,
        parse_identifier,
        parse_operator,
        parse_parens,
    ))(i)
}

fn flatten_words(words: Vec<Vec<Word>>) -> Vec<Word> {
    words.into_iter().flatten().collect()
}

// a compound word is something like -x or x+y -- something that *looks* like a
// single word but actually should be split up. There is *currently* no way to
// define user-specified compound symbols; they are all baked into the parser.
//
// this should eventually support something like +.*, right?
fn parse_compound_word<'a>(i: &'a str) -> IResult<&'a str, Vec<Word>> {
    // try to parse one thing. then, after you parse one thing, try to
    // *greedily* parse a subtraction operator. then return to parsing one more
    // thing. does that work? let's find out.
    map(
        many1(
            tuple((parse_word, opt(char('-')))).map(|(word, subtraction)| {
                // TODO: should this also be some kind of builtin?
                if subtraction.is_some() {
                    vec![word, Word::Identifier("-".to_string())]
                } else {
                    vec![word]
                }
            }),
        ),
        flatten_words,
    )(i)
}

fn parse_words<'a>(i: &'a str) -> IResult<&'a str, Vec<Word>> {
    map(
        separated_list0(multispace1, parse_compound_word),
        flatten_words,
    )(i)
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_literal() {
        k9::snapshot!(
            parse_word("9223372036854775807"),
            r#"Ok(("", Int64(9223372036854775807)))"#
        );
        k9::snapshot!(
            parse_word("-9223372036854775807"),
            r#"Ok(("", Num(-9223372036854775807)))"#
        );
        // TODO: this should parse as a bigint literal
        k9::snapshot!(
            parse_word("9223372036854775808"),
            r#"Err(Error(Error { input: "9223372036854775808", code: Alpha }))"#
        );
        // TODO: obviously this should parse correctly as an int
        k9::snapshot!(
            parse_word("-9223372036854775808"),
            r#"Err(Error(Error { input: "-9223372036854775808", code: Alpha }))"#
        );
    }

    #[test]
    fn parse_identifier() {
        k9::snapshot!(parse_word("hi"), r#"Ok(("", Identifier("hi")))"#);
    }

    fn short_parse(input: &str) -> String {
        let (remaining, parsed) = parse_words(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        Word::Parens(parsed).to_short_string()
    }

    #[test]
    fn parse_hyphens() {
        k9::snapshot!(short_parse("-x"), "((neg x))");
        k9::snapshot!(short_parse("--x"), "((neg (neg x)))");
        k9::snapshot!(short_parse("- x"), "(- x)");
        k9::snapshot!(short_parse("- -x"), "(- (neg x))");
        k9::snapshot!(short_parse("x-y"), "(x - y)");
        k9::snapshot!(short_parse("-x-y"), "((neg x) - y)");
        k9::snapshot!(short_parse("x--y"), "(x - (neg y))");
        k9::snapshot!(short_parse("-x--y"), "((neg x) - (neg y))");
        k9::snapshot!(short_parse("--x--y"), "((neg (neg x)) - (neg y))");
        k9::snapshot!(short_parse("x*-y"), "(x * - y)");
        k9::snapshot!(short_parse("x-*y"), "(x - * y)");
        k9::snapshot!(short_parse("x* -y"), "(x * (neg y))");
        k9::snapshot!(short_parse("x*(-y)"), "(x * ((neg y)))");
    }
}
