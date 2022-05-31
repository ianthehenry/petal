use crate::helpers::*;
use crate::located_token::*;
use crate::span::*;
use crate::token::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{anychar, char, line_ending, space0, space1},
    combinator::{eof, map, opt, recognize, verify},
    multi::many0,
    sequence::tuple,
    IResult,
};
use std::cmp::Ordering;

fn is_initial_identifier_character(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_identifier_character(c: char) -> bool {
    is_initial_identifier_character(c) || c.is_numeric()
}

fn identifier(i: Span) -> IResult<Span, LocatedToken> {
    map(
        recognize(tuple((
            verify(anychar, |c| is_initial_identifier_character(*c)),
            take_while(is_identifier_character),
        ))),
        LocatedToken::build_string(Token::Identifier),
    )(i)
}

fn numeric_literal(i: Span) -> IResult<Span, LocatedToken> {
    map(
        recognize(tuple((opt(char('-')), take_while1(char::is_numeric)))),
        LocatedToken::build_string(Token::NumericLiteral),
    )(i)
}

fn punctuation_soup(i: Span) -> IResult<Span, LocatedToken> {
    fn is_operator_punctuation(c: char) -> bool {
        !(c.is_whitespace() || c.is_alphabetic() || c.is_numeric() || "()[];\"'#_".contains(c))
    }
    map(
        verify(take_while1(is_operator_punctuation), |s: &Span| **s != "="),
        LocatedToken::build_string(Token::PunctuationSoup),
    )(i)
}

fn semicolons(i: Span) -> IResult<Span, LocatedToken> {
    map(
        take_while1(|c| c == ';'),
        LocatedToken::build(|x| Token::Semicolons(x.len())),
    )(i)
}

fn token(i: Span) -> IResult<Span, LocatedToken> {
    use Token::*;
    alt((
        identifier,
        numeric_literal,
        punctuation_soup,
        semicolons,
        map(tag("("), LocatedToken::build_const(OpenParen)),
        map(tag(")"), LocatedToken::build_const(CloseParen)),
        map(tag("["), LocatedToken::build_const(OpenBracket)),
        map(tag("]"), LocatedToken::build_const(CloseBracket)),
        map(tag("="), LocatedToken::build_const(EqualSign)),
        map(space1, LocatedToken::build_const(Space)),
    ))(i)
}

fn eol(i: Span) -> IResult<Span, ()> {
    ignore(alt((line_ending, eof)))(i)
}

pub(super) fn tokenize_lines(i: Span) -> IResult<Span, Vec<LocatedToken>> {
    let mut result = Vec::new();
    let mut indentation_stack: Vec<usize> = vec![0];

    let mut remaining = i;

    while !remaining.is_empty() {
        let i = remaining;

        // skip over completely blank lines, so that editors that trim trailing
        // whitespace don't interfere with indentation.
        // TODO: should this also skip over lines that consist only of
        // whitespace? probably, right?
        if let Ok((i, ())) = eol(i) {
            remaining = i;
            continue;
        }

        let (i, spaces) = recognize(space0)(i)?;
        let this_indentation = spaces.len();
        let previous_indentation = *indentation_stack.last().unwrap();
        // TODO: this shouldn't push indent/outdent for blank lines
        match this_indentation.cmp(&previous_indentation) {
            Ordering::Greater => {
                indentation_stack.push(this_indentation);
                result.push(LocatedToken::of_span(spaces, Token::Indent));
            }
            Ordering::Less => loop {
                let candidate = *indentation_stack.last().unwrap();
                match candidate.cmp(&this_indentation) {
                    Ordering::Less => {
                        // TODO: this should return a custom error for an illegal outdent
                        return Err(nom::Err::Error(nom::error::Error::<Span>::new(
                            i,
                            nom::error::ErrorKind::Fail,
                        )));
                    }
                    Ordering::Greater => {
                        result.push(LocatedToken::of_span(spaces, Token::Outdent));
                        indentation_stack.pop();
                    }
                    Ordering::Equal => break,
                }
            },
            Ordering::Equal => (),
        }
        let (i, tokens) = many0(token)(i)?;
        result.extend(tokens);

        // we always add a newline, even if it isn't present in the source
        let (i, eol) = recognize(eol)(i)?;
        result.push(LocatedToken::of_span(eol, Token::Newline));
        remaining = i;
    }

    for _ in 0..(indentation_stack.len() - 1) {
        // by putting this inside the loop we know the unwrap is safe, since we
        // can't add anything to the indentation stack without also adding
        // something to the result vector
        let eof = result.last().unwrap().location.clone();
        result.push(LocatedToken::new(eof, Token::Outdent))
    }

    Ok((remaining, result))
}

// TODO: comments, string literals, etc. certain types of comments are actually
// significant...
pub(super) fn tokenize(i: &str) -> Vec<LocatedToken> {
    let i = new_span(i);
    let (remaining, tokens) = tokenize_lines(i).unwrap();
    if !remaining.is_empty() {
        panic!("tokenize error, remaining: {:?}", remaining);
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(input: &str) -> String {
        let tokens = tokenize(input);
        tokens
            .iter()
            .map(|t: &LocatedToken| format!("{}", t.token))
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn spacing() {
        k9::snapshot!(
            test(
                "
x=10
x =10
x= 10
x = 10
"
            ),
            "x = 10 ␤ x ␠ = 10 ␤ x = ␠ 10 ␤ x ␠ = ␠ 10 ␤"
        );

        k9::snapshot!(
            test(
                "
<=10
< =10
<= 10
< = 10
"
            ),
            "<= 10 ␤ < ␠ = 10 ␤ <= ␠ 10 ␤ < ␠ = ␠ 10 ␤"
        );
    }

    #[test]
    fn identifiers() {
        k9::snapshot!(test("1x x1 x1y"), "1 x ␠ x1 ␠ x1y ␤");
    }

    #[test]
    fn numeric_literals() {
        k9::snapshot!(test("9223372036854775807"), "9223372036854775807 ␤");
        k9::snapshot!(test("-9223372036854775807"), "-9223372036854775807 ␤");
        k9::snapshot!(test("9223372036854775808"), "9223372036854775808 ␤");
        k9::snapshot!(test("-9223372036854775808"), "-9223372036854775808 ␤");
    }

    #[test]
    fn semicolons() {
        k9::snapshot!(test("[x;y]"), "[ x ; y ] ␤");
        k9::snapshot!(test("[x;;y]"), "[ x ;; y ] ␤");
        k9::snapshot!(test("[x;y;;z]"), "[ x ; y ;; z ] ␤");
        k9::snapshot!(test("[x;; ; ;;;y]"), "[ x ;; ␠ ; ␠ ;;; y ] ␤");
    }

    #[test]
    fn always_ends_with_newline() {
        k9::snapshot!(test("x=10"), "x = 10 ␤");
    }

    #[test]
    fn multiple_newlines_collapse() {
        k9::snapshot!(
            test(
                "


x


y




"
            ),
            "x ␤ y ␤"
        );
    }

    #[test]
    fn indentation() {
        k9::snapshot!(
            test(
                "
a
  b
  c
d
  e
    f
g
"
            ),
            "a ␤ → b ␤ c ␤ ← d ␤ → e ␤ → f ␤ ← ← g ␤"
        );
    }

    #[test]
    fn illegal_outdent() {
        k9::snapshot!(
            tokenize_lines(new_span(
                "
a
  b
 c
d
"
            )),
            r#"
Err(
    Error(
        Error {
            input: LocatedSpan {
                offset: 8,
                line: 4,
                fragment: "c
d
",
                extra: (),
            },
            code: Fail,
        },
    ),
)
"#
        );
    }
}