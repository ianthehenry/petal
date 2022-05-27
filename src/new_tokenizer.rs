use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::{
        complete::{
            alpha1, char, digit1, line_ending, multispace1, one_of, satisfy, space0, space1,
        },
        streaming::none_of,
    },
    combinator::{eof, fail, map, map_res, opt, success, verify},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Atom(&'a str),
    EqualSign,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Semicolons(usize),
    Space,
    Newline,
    Indent,
    Outdent,
}

// TODO: comments, string literals, etc
fn atom(i: &str) -> IResult<&str, Token> {
    let pred = |c| !(char::is_whitespace(c) || "()[];".contains(c));
    map(
        verify(take_while(pred), |s: &str| !s.is_empty() && s != "="),
        |op| Token::Atom(op),
    )(i)
}

fn semicolons(i: &str) -> IResult<&str, Token> {
    map(many1(char(';')), |semis| Token::Semicolons(semis.len()))(i)
}

fn token(i: &str) -> IResult<&str, Token> {
    use Token::*;
    alt((
        atom,
        semicolons,
        replace(char('('), OpenParen),
        replace(char(')'), CloseParen),
        replace(char('['), OpenBracket),
        replace(char(']'), CloseBracket),
        replace(char('='), EqualSign),
        replace(space1, Space),
    ))(i)
}

fn tokens(i: &str) -> IResult<&str, Vec<Token>> {
    separated_list0(multispace1, token)(i)
}

fn eol(i: &str) -> IResult<&str, ()> {
    ignore(alt((line_ending, eof)))(i)
}

fn newline(i: &str) -> IResult<&str, ()> {
    ignore(line_ending)(i)
}

fn parse_lines(i: &str) -> IResult<&str, Vec<Token>> {
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

        let (i, spaces) = space0(i)?;
        let this_indentation = spaces.len();
        let previous_indentation = *indentation_stack.last().unwrap();
        if this_indentation > previous_indentation {
            indentation_stack.push(this_indentation);
            result.push(Token::Indent);
        } else if this_indentation < previous_indentation {
            loop {
                let candidate = *indentation_stack.last().unwrap();
                if candidate < this_indentation {
                    // TODO: this should return a custom error for an illegal outdent
                    return Err(nom::Err::Error(nom::error::Error::<&str>::new(
                        i,
                        nom::error::ErrorKind::Fail,
                    )));
                } else if candidate > this_indentation {
                    result.push(Token::Outdent);
                    indentation_stack.pop();
                } else if candidate == this_indentation {
                    break;
                }
            }
        }
        let (i, tokens) = many0(token)(i)?;
        result.extend(tokens);

        if let Ok((i, _)) = newline(i) {
            result.push(Token::Newline);
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
    let (remaining, tokens) = parse_lines(i).unwrap();
    if !remaining.is_empty() {
        panic!("tokenize error, remaining: {:?}", remaining);
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<'a> Token<'a> {
        fn to_short_string(&self) -> String {
            match self {
                Token::Atom(atom) => atom.to_string(),
                Token::OpenParen => "(".to_string(),
                Token::CloseParen => ")".to_string(),
                Token::Space => "␠".to_string(),
                Token::Newline => "␤".to_string(),
                Token::Semicolons(count) => ";".repeat(*count),
                Token::OpenBracket => "[".to_string(),
                Token::CloseBracket => "]".to_string(),
                Token::EqualSign => "=".to_string(),
                Token::Indent => "→".to_string(),
                Token::Outdent => "←".to_string(),
            }
        }
    }

    fn test(input: &str) -> String {
        let tokens = tokenize(input);
        tokens
            .iter()
            .map(|x| x.to_short_string())
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
            "x=10 ␤ x ␠ =10 ␤ x= ␠ 10 ␤ x ␠ = ␠ 10 ␤"
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
            parse_lines(
                "
a
  b
 c
d
"
            ),
            r#"
Err(Error(Error { input: "c
d
", code: Fail }))
"#
        );
    }
}
