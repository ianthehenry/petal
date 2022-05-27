use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, line_ending, space0, space1},
    combinator::{eof, map, opt, recognize, verify},
    multi::many0,
    sequence::tuple,
    IResult, Parser,
};
use nom_locate::LocatedSpan;

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
// TODO: comments, string literals, etc. certain types of comments are actually
// significant...
pub struct Token<'a> {
    span: Span<'a>,
    type_: TokenType,
}

impl<'a> Token<'a> {
    fn new(span: Span<'a>, type_: TokenType) -> Self {
        Token { span, type_ }
    }

    fn build(type_: TokenType) -> impl Fn(Span<'a>) -> Token<'a> {
        move |span: Span| Token { span, type_ }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

        if let Ok((i, nl)) = recognize(newline)(i) {
            result.push(Token::new(nl, TokenType::Newline));
            remaining = i;
        } else {
            let (i, ()) = ignore(eof)(i)?;
            remaining = i;
        }
    }

    Ok((remaining, result))
}

fn replace<I, O1, O2, E, F>(mut parser: F, value: O2) -> impl FnMut(I) -> IResult<I, O2, E>
where
    O2: Clone,
    F: Parser<I, O1, E>,
{
    move |input: I| {
        let (input, _) = parser.parse(input)?;
        Ok((input, value.clone()))
    }
}

fn ignore<I, O, E, F>(parser: F) -> impl FnMut(I) -> IResult<I, (), E>
where
    F: Parser<I, O, E>,
{
    replace(parser, ())
}

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

    // TODO: this shouldn't be allowed. this is an illegal outdent.
    // it should keep a stack of the current indentation and only allow
    // it to outdent to a particular past level of indentation. it should
    // also emit multiple outdents when outdenting across multiple levels.
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
