use std::num::NonZeroUsize;

use crate::helpers::*;
use nom::{
    branch::alt,
    bytes::complete::{take, take_while1},
    combinator::{eof, map, map_opt, opt, verify},
    multi::many1,
    IResult, InputLength, InputTake,
};

use crate::new_tokenizer::{LocatedToken, Token};

type ParseResult<'a, R> = IResult<Tokens<'a>, R>;
type TokenResult<'a> = ParseResult<'a, &'a LocatedToken>;
type UnitResult<'a> = ParseResult<'a, ()>;

#[derive(Debug, Clone)]
struct Tokens<'a>(&'a [LocatedToken]);

impl<'a> nom::InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Tokens(&self.0[0..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (Tokens(suffix), Tokens(prefix))
    }
}

impl<'a> nom::InputTakeAtPosition for Tokens<'a> {
    type Item = &'a LocatedToken;

    fn split_at_position<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(i) => Ok(self.take_split(i)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position1(predicate, e) {
            Err(nom::Err::Incomplete(_)) => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
            res => res,
        }
    }
}

impl<'a> nom::InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl nom::InputLength for LocatedToken {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> nom::InputIter for Tokens<'a> {
    type Item = &'a LocatedToken;
    type Iter = std::iter::Enumerate<Self::IterElem>;
    type IterElem = std::slice::Iter<'a, LocatedToken>;

    fn iter_indices(&self) -> Self::Iter {
        self.0.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        let len = self.0.len();
        if len >= count {
            Ok(count)
        } else {
            let needed = unsafe { NonZeroUsize::new_unchecked(count - len) };
            Err(nom::Needed::Size(needed))
        }
    }
}

fn any_token(i: Tokens) -> TokenResult {
    map(take(1usize), |tokens: Tokens| &tokens.0[0])(i)
}

fn identifier(i: Tokens) -> ParseResult<String> {
    map_opt(any_token, |t: &LocatedToken| match &t.token {
        Token::Identifier(x) => Some(x.to_string()),
        _ => None,
    })(i)
}

fn token_matching<'a>(token: Token) -> impl FnMut(Tokens<'a>) -> TokenResult<'a> {
    verify(any_token, move |t: &LocatedToken| t.token == token)
}

fn skip_token<'a>(token: Token) -> impl FnMut(Tokens<'a>) -> UnitResult {
    ignore(verify(any_token, move |t: &LocatedToken| t.token == token))
}

fn maybe_space(i: Tokens) -> UnitResult {
    ignore(opt(skip_token(Token::Space)))(i)
}

fn expressions(i: Tokens) -> ParseResult<Expression> {
    map(
        take_while1(|t: &LocatedToken| match t.token {
            Token::Identifier(_) => true,
            Token::PunctuationSoup(_) => true,
            Token::NumericLiteral(_) => true,
            Token::OpenParen => true,
            Token::CloseParen => true,
            Token::OpenBracket => true,
            Token::CloseBracket => true,
            Token::Semicolons(_) => true,
            Token::Space => true,
            Token::EqualSign => false,
            Token::Newline => false,
            Token::Indent => false,
            Token::Outdent => false,
        }),
        |tokens: Tokens| Vec::from(tokens.0),
    )(i)
}

fn assignment_statement(i: Tokens) -> ParseResult<Statement> {
    let (i, identifier) = identifier(i)?;
    let (i, ()) = maybe_space(i)?;
    let (i, ()) = skip_token(Token::EqualSign)(i)?;
    let (i, ()) = maybe_space(i)?;
    let (i, expression_tokens) = opt(expressions)(i)?;
    let (i, ()) = skip_token(Token::Newline)(i)?;

    if let Ok((i, ())) = skip_token(Token::Indent)(i.clone()) {
        let (i, mut block) = statements(i)?;
        if let Some(expression_tokens) = expression_tokens {
            block.insert(0, Statement::Expression(expression_tokens));
        };
        Ok((i, Statement::CompoundAssignment(identifier, block)))
    } else {
        if let Some(expression_tokens) = expression_tokens {
            Ok((
                i,
                Statement::SimpleAssignment(identifier, expression_tokens),
            ))
        } else {
            // TODO: should be a custom error type
            Err(nom::Err::Error(nom::error::Error::new(
                i,
                nom::error::ErrorKind::Fail,
            )))
        }
    }
}

fn expression_statement(i: Tokens) -> ParseResult<Statement> {
    let (i, expressions) = map(expressions, Statement::Expression)(i)?;
    let (i, ()) = skip_token(Token::Newline)(i)?;
    Ok((i, expressions))
}

fn statement(i: Tokens) -> ParseResult<Statement> {
    alt((assignment_statement, expression_statement))(i)
}

// separated by newlines until either outdent or EOF reached
fn statements(i: Tokens) -> ParseResult<Vec<Statement>> {
    let (i, statements) = many1(statement)(i)?;
    let (i, ()) = alt((skip_token(Token::Outdent), ignore(eof)))(i)?;
    Ok((i, statements))
}

type Block = Vec<Statement>;
type Expression = Vec<LocatedToken>;

#[derive(Debug)]
enum Statement {
    SimpleAssignment(String, Expression),
    CompoundAssignment(String, Block),
    Expression(Expression),
}

#[cfg(test)]
mod tests {
    use crate::new_tokenizer::tokenize;

    use super::*;

    fn test(input: &'static str) -> String {
        let tokens = tokenize(input);
        let tokens = Tokens(&tokens);
        format!("{:#?}", statements(tokens))
    }

    #[test]
    fn simple_assignment() {
        k9::snapshot!(
            test("foo = bar"),
            r#"
Ok(
    (
        Tokens(
            [],
        ),
        [
            SimpleAssignment(
                "foo",
                [
                    LocatedToken {
                        location: Location {
                            offset: 6,
                            line: 1,
                        },
                        token: Identifier(
                            "bar",
                        ),
                    },
                ],
            ),
        ],
    ),
)
"#
        );
    }

    #[test]
    fn compound_assignment() {
        k9::snapshot!(
            test(
                "
foo =
  x = 10
  x * 2
"
            ),
            r#"
Ok(
    (
        Tokens(
            [],
        ),
        [
            CompoundAssignment(
                "foo",
                [
                    SimpleAssignment(
                        "x",
                        [
                            LocatedToken {
                                location: Location {
                                    offset: 13,
                                    line: 3,
                                },
                                token: NumericLiteral(
                                    "10",
                                ),
                            },
                        ],
                    ),
                    Expression(
                        [
                            LocatedToken {
                                location: Location {
                                    offset: 18,
                                    line: 4,
                                },
                                token: Identifier(
                                    "x",
                                ),
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 19,
                                    line: 4,
                                },
                                token: Space,
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 20,
                                    line: 4,
                                },
                                token: PunctuationSoup(
                                    "*",
                                ),
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 21,
                                    line: 4,
                                },
                                token: Space,
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 22,
                                    line: 4,
                                },
                                token: NumericLiteral(
                                    "2",
                                ),
                            },
                        ],
                    ),
                ],
            ),
        ],
    ),
)
"#
        );
    }

    #[test]
    fn compound_assignment_with_initial_expression() {
        k9::snapshot!(
            test(
                "
foo = x + y
  x = 10
  y = 20
  x - y
"
            ),
            r#"
Ok(
    (
        Tokens(
            [],
        ),
        [
            CompoundAssignment(
                "foo",
                [
                    Expression(
                        [
                            LocatedToken {
                                location: Location {
                                    offset: 7,
                                    line: 2,
                                },
                                token: Identifier(
                                    "x",
                                ),
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 8,
                                    line: 2,
                                },
                                token: Space,
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 9,
                                    line: 2,
                                },
                                token: PunctuationSoup(
                                    "+",
                                ),
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 10,
                                    line: 2,
                                },
                                token: Space,
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 11,
                                    line: 2,
                                },
                                token: Identifier(
                                    "y",
                                ),
                            },
                        ],
                    ),
                    SimpleAssignment(
                        "x",
                        [
                            LocatedToken {
                                location: Location {
                                    offset: 19,
                                    line: 3,
                                },
                                token: NumericLiteral(
                                    "10",
                                ),
                            },
                        ],
                    ),
                    SimpleAssignment(
                        "y",
                        [
                            LocatedToken {
                                location: Location {
                                    offset: 28,
                                    line: 4,
                                },
                                token: NumericLiteral(
                                    "20",
                                ),
                            },
                        ],
                    ),
                    Expression(
                        [
                            LocatedToken {
                                location: Location {
                                    offset: 33,
                                    line: 5,
                                },
                                token: Identifier(
                                    "x",
                                ),
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 34,
                                    line: 5,
                                },
                                token: Space,
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 35,
                                    line: 5,
                                },
                                token: PunctuationSoup(
                                    "-",
                                ),
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 36,
                                    line: 5,
                                },
                                token: Space,
                            },
                            LocatedToken {
                                location: Location {
                                    offset: 37,
                                    line: 5,
                                },
                                token: Identifier(
                                    "y",
                                ),
                            },
                        ],
                    ),
                ],
            ),
        ],
    ),
)
"#
        );
    }

    #[test]
    fn nested_compound_assignment() {
        k9::snapshot!(
            test(
                "
foo =
  x =
    y =
      10
"
            ),
            r#"
Ok(
    (
        Tokens(
            [],
        ),
        [
            CompoundAssignment(
                "foo",
                [
                    CompoundAssignment(
                        "x",
                        [
                            CompoundAssignment(
                                "y",
                                [
                                    Expression(
                                        [
                                            LocatedToken {
                                                location: Location {
                                                    offset: 27,
                                                    line: 5,
                                                },
                                                token: NumericLiteral(
                                                    "10",
                                                ),
                                            },
                                        ],
                                    ),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
        ],
    ),
)
"#
        );
    }

    #[test]
    fn parse_errors() {
        k9::snapshot!(
            test("foo = bar = baz"),
            r#"
Err(
    Error(
        Error {
            input: Tokens(
                [
                    LocatedToken {
                        location: Location {
                            offset: 4,
                            line: 1,
                        },
                        token: EqualSign,
                    },
                    LocatedToken {
                        location: Location {
                            offset: 5,
                            line: 1,
                        },
                        token: Space,
                    },
                    LocatedToken {
                        location: Location {
                            offset: 6,
                            line: 1,
                        },
                        token: Identifier(
                            "bar",
                        ),
                    },
                    LocatedToken {
                        location: Location {
                            offset: 9,
                            line: 1,
                        },
                        token: Space,
                    },
                    LocatedToken {
                        location: Location {
                            offset: 10,
                            line: 1,
                        },
                        token: EqualSign,
                    },
                    LocatedToken {
                        location: Location {
                            offset: 11,
                            line: 1,
                        },
                        token: Space,
                    },
                    LocatedToken {
                        location: Location {
                            offset: 12,
                            line: 1,
                        },
                        token: Identifier(
                            "baz",
                        ),
                    },
                    LocatedToken {
                        location: Location {
                            offset: 15,
                            line: 1,
                        },
                        token: Newline,
                    },
                ],
            ),
            code: Verify,
        },
    ),
)
"#
        );

        k9::snapshot!(
            test("foo ="),
            "
Err(
    Error(
        Error {
            input: Tokens(
                [
                    LocatedToken {
                        location: Location {
                            offset: 4,
                            line: 1,
                        },
                        token: EqualSign,
                    },
                    LocatedToken {
                        location: Location {
                            offset: 5,
                            line: 1,
                        },
                        token: Newline,
                    },
                ],
            ),
            code: Verify,
        },
    ),
)
"
        );
    }
}
