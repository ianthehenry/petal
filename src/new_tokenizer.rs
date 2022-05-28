use crate::helpers::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, line_ending, space0, space1},
    combinator::{eof, map, opt, recognize, verify},
    multi::many0,
    sequence::tuple,
    IResult,
};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Eq, Clone)]
pub struct Token<'a> {
    pub span: Span<'a>,
    pub type_: TokenType,
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.span.fragment() == other.span.fragment() && self.type_ == other.type_
    }
}

impl<'a> Token<'a> {
    fn new(span: Span<'a>, type_: TokenType) -> Self {
        Token { span, type_ }
    }

    fn build(type_: TokenType) -> impl Fn(Span<'a>) -> Token<'a> {
        move |span: Span| Token { span, type_ }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenType {
    Identifier,
    PunctuationSoup,
    NumericLiteral,
    EqualSign,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Semicolons,
    Space,
    Newline,
    Indent,
    Outdent,
}

fn identifier(i: Span) -> IResult<Span, Token> {
    map(
        take_while1(char::is_alphabetic),
        Token::build(TokenType::Identifier),
    )(i)
}

fn numeric_literal(i: Span) -> IResult<Span, Token> {
    map(
        recognize(tuple((opt(char('-')), take_while1(char::is_numeric)))),
        Token::build(TokenType::NumericLiteral),
    )(i)
}

fn punctuation_soup<'a>(i: Span<'a>) -> IResult<Span<'a>, Token<'a>> {
    fn is_operator_punctuation(c: char) -> bool {
        !(c.is_whitespace() || c.is_alphabetic() || c.is_numeric() || "()[];".contains(c))
    }
    map(
        verify(take_while1(is_operator_punctuation), |s: &Span| **s != "="),
        Token::build(TokenType::PunctuationSoup),
    )(i)
}

fn semicolons(i: Span) -> IResult<Span, Token> {
    map(
        take_while1(|c| c == ';'),
        Token::build(TokenType::Semicolons),
    )(i)
}

fn token(i: Span) -> IResult<Span, Token> {
    use TokenType::*;
    alt((
        identifier,
        numeric_literal,
        punctuation_soup,
        semicolons,
        map(tag("("), Token::build(OpenParen)),
        map(tag(")"), Token::build(CloseParen)),
        map(tag("["), Token::build(OpenBracket)),
        map(tag("]"), Token::build(CloseBracket)),
        map(tag("="), Token::build(EqualSign)),
        map(space1, Token::build(Space)),
    ))(i)
}

fn eol(i: Span) -> IResult<Span, ()> {
    ignore(alt((line_ending, eof)))(i)
}

fn newline(i: Span) -> IResult<Span, ()> {
    ignore(line_ending)(i)
}

pub fn parse_lines(i: Span) -> IResult<Span, Vec<Token>> {
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
        if this_indentation > previous_indentation {
            indentation_stack.push(this_indentation);
            result.push(Token::new(spaces, TokenType::Indent));
        } else if this_indentation < previous_indentation {
            loop {
                let candidate = *indentation_stack.last().unwrap();
                if candidate < this_indentation {
                    // TODO: this should return a custom error for an illegal outdent
                    return Err(nom::Err::Error(nom::error::Error::<Span>::new(
                        i,
                        nom::error::ErrorKind::Fail,
                    )));
                } else if candidate > this_indentation {
                    result.push(Token::new(spaces.clone(), TokenType::Outdent));
                    indentation_stack.pop();
                } else if candidate == this_indentation {
                    break;
                }
            }
        }
        let (i, tokens) = many0(token)(i)?;
        result.extend(tokens);

        // we always add a newline, even if it isn't present in the source
        let (i, eol) = recognize(eol)(i)?;
        result.push(Token::new(eol, TokenType::Newline));
        remaining = i;
    }

    for i in 0..(indentation_stack.len() - 1) {
        let eof = result.last().unwrap().span;
        result.push(Token::new(eof, TokenType::Outdent))
    }

    Ok((remaining, result))
}

// TODO: comments, string literals, etc. certain types of comments are actually
// significant...
pub fn tokenize(i: &str) -> Vec<Token> {
    let i = LocatedSpan::new(i);
    let (remaining, tokens) = parse_lines(i).unwrap();
    if !remaining.is_empty() {
        panic!("tokenize error, remaining: {:?}", remaining);
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    fn show_token(token: &Token) -> String {
        use TokenType::*;
        match token.type_ {
            Space => "␠".to_string(),
            Newline => "␤".to_string(),
            Indent => "→".to_string(),
            Outdent => "←".to_string(),
            _ => token.span.fragment().to_string(),
        }
    }

    fn test(input: &str) -> String {
        let tokens = tokenize(input);
        tokens.iter().map(show_token).collect::<Vec<_>>().join(" ")
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
    fn always_ends_with_newline() {
        k9::snapshot!(test("x=10"), "x = 10 ␤");
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
            parse_lines(LocatedSpan::new(
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
