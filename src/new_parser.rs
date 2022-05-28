use std::num::NonZeroUsize;

use crate::helpers::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while, take_while1},
    character::complete::{char, line_ending, space0, space1},
    combinator::{eof, map, opt, peek, recognize, verify},
    multi::{many0, many1},
    sequence::tuple,
    IResult, InputLength, InputTake, Parser,
};

use crate::new_tokenizer::{Span, Token, TokenType};

type ParseResult<'a, 't, R> = IResult<Tokens<'a, 't>, R>;
type TokenResult<'a, 't> = ParseResult<'a, 't, &'a Token<'t>>;
type UnitResult<'a, 't> = ParseResult<'a, 't, ()>;

#[derive(Debug, Clone)]
struct Tokens<'a, 't>(&'a [Token<'t>]);

impl<'a, 't> nom::InputTake for Tokens<'a, 't> {
    fn take(&self, count: usize) -> Self {
        Tokens(&self.0[0..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (Tokens(suffix), Tokens(prefix))
    }
}

impl<'a, 't> nom::InputTakeAtPosition for Tokens<'a, 't> {
    type Item = &'a Token<'t>;

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

impl<'a, 't> nom::InputLength for Tokens<'a, 't> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> nom::InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a, 't> nom::InputIter for Tokens<'a, 't> {
    type Item = &'a Token<'t>;
    type Iter = std::iter::Enumerate<Self::IterElem>;
    type IterElem = std::slice::Iter<'a, Token<'t>>;

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

fn any_token<'a, 't>(i: Tokens<'a, 't>) -> TokenResult<'a, 't> {
    map(take(1usize), |tokens: Tokens<'a, 't>| &tokens.0[0])(i)
}

fn token_of_type<'a, 't: 'a>(
    type_: TokenType,
) -> impl FnMut(Tokens<'a, 't>) -> TokenResult<'a, 't> {
    verify(any_token, move |token: &Token<'t>| token.type_ == type_)
}

fn token_matching<'a, 't: 'a, 's: 'a>(
    type_: TokenType,
    value: &'s str,
) -> impl FnMut(Tokens<'a, 't>) -> TokenResult<'a, 't> {
    verify(any_token, move |token: &Token<'t>| {
        token.type_ == type_ && *token.span.fragment() == value
    })
}

fn ignore_type<'a, 't: 'a>(type_: TokenType) -> impl FnMut(Tokens<'a, 't>) -> UnitResult<'a, 't> {
    ignore(token_of_type(type_))
}

fn maybe_space<'a, 't>(i: Tokens<'a, 't>) -> UnitResult<'a, 't> {
    ignore(opt(token_of_type(TokenType::Space)))(i)
}

fn expressions<'a, 't>(i: Tokens<'a, 't>) -> ParseResult<'a, 't, Expression<'t>> {
    map(
        take_while1(|x: &Token| match x.type_ {
            TokenType::Identifier => true,
            TokenType::PunctuationSoup => true,
            TokenType::NumericLiteral => true,
            TokenType::OpenParen => true,
            TokenType::CloseParen => true,
            TokenType::OpenBracket => true,
            TokenType::CloseBracket => true,
            TokenType::Semicolons => true,
            TokenType::Space => true,
            TokenType::EqualSign => false,
            TokenType::Newline => false,
            TokenType::Indent => false,
            TokenType::Outdent => false,
        }),
        |tokens: Tokens<'a, 't>| Vec::from(tokens.0),
    )(i)
}

fn assignment_statement<'a, 't>(i: Tokens<'a, 't>) -> ParseResult<'a, 't, Statement<'t>> {
    let (i, identifier_token) = token_of_type(TokenType::Identifier)(i)?;
    let (i, ()) = maybe_space(i)?;
    let (i, ()) = ignore_type(TokenType::EqualSign)(i)?;
    let (i, ()) = maybe_space(i)?;
    let (i, expression_tokens) = opt(expressions)(i)?;
    let (i, ()) = ignore_type(TokenType::Newline)(i)?;

    if let Ok((i, ())) = ignore_type(TokenType::Indent)(i.clone()) {
        let (i, mut block) = statements(i)?;
        if let Some(expression_tokens) = expression_tokens {
            block.insert(0, Statement::Expression(expression_tokens));
        };
        Ok((
            i,
            Statement::CompoundAssignment(identifier_token.span.fragment().to_string(), block),
        ))
    } else {
        if let Some(expression_tokens) = expression_tokens {
            Ok((
                i,
                Statement::SimpleAssignment(
                    identifier_token.span.fragment().to_string(),
                    expression_tokens,
                ),
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

fn expression_statement<'a, 't>(i: Tokens<'a, 't>) -> ParseResult<'a, 't, Statement<'t>> {
    let (i, expressions) = map(expressions, Statement::Expression)(i)?;
    let (i, ()) = ignore_type(TokenType::Newline)(i)?;
    Ok((i, expressions))
}

fn statement<'a, 't>(i: Tokens<'a, 't>) -> ParseResult<'a, 't, Statement<'t>> {
    alt((assignment_statement, expression_statement))(i)
}

// separated by newlines until either outdent or EOF reached
fn statements<'a, 't>(i: Tokens<'a, 't>) -> ParseResult<'a, 't, Vec<Statement<'t>>> {
    let (i, statements) = many1(statement)(i)?;
    let (i, ()) = alt((ignore_type(TokenType::Outdent), ignore(eof)))(i)?;
    Ok((i, statements))
}

type Block<'a> = Vec<Statement<'a>>;
type Expression<'a> = Vec<Token<'a>>;

#[derive(Debug)]
enum Statement<'a> {
    SimpleAssignment(String, Expression<'a>),
    CompoundAssignment(String, Block<'a>),
    Expression(Expression<'a>),
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
                    Token {
                        span: LocatedSpan {
                            offset: 6,
                            line: 1,
                            fragment: "bar",
                            extra: (),
                        },
                        type_: Identifier,
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
                            Token {
                                span: LocatedSpan {
                                    offset: 13,
                                    line: 3,
                                    fragment: "10",
                                    extra: (),
                                },
                                type_: NumericLiteral,
                            },
                        ],
                    ),
                    Expression(
                        [
                            Token {
                                span: LocatedSpan {
                                    offset: 18,
                                    line: 4,
                                    fragment: "x",
                                    extra: (),
                                },
                                type_: Identifier,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 19,
                                    line: 4,
                                    fragment: " ",
                                    extra: (),
                                },
                                type_: Space,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 20,
                                    line: 4,
                                    fragment: "*",
                                    extra: (),
                                },
                                type_: PunctuationSoup,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 21,
                                    line: 4,
                                    fragment: " ",
                                    extra: (),
                                },
                                type_: Space,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 22,
                                    line: 4,
                                    fragment: "2",
                                    extra: (),
                                },
                                type_: NumericLiteral,
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
                            Token {
                                span: LocatedSpan {
                                    offset: 7,
                                    line: 2,
                                    fragment: "x",
                                    extra: (),
                                },
                                type_: Identifier,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 8,
                                    line: 2,
                                    fragment: " ",
                                    extra: (),
                                },
                                type_: Space,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 9,
                                    line: 2,
                                    fragment: "+",
                                    extra: (),
                                },
                                type_: PunctuationSoup,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 10,
                                    line: 2,
                                    fragment: " ",
                                    extra: (),
                                },
                                type_: Space,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 11,
                                    line: 2,
                                    fragment: "y",
                                    extra: (),
                                },
                                type_: Identifier,
                            },
                        ],
                    ),
                    SimpleAssignment(
                        "x",
                        [
                            Token {
                                span: LocatedSpan {
                                    offset: 19,
                                    line: 3,
                                    fragment: "10",
                                    extra: (),
                                },
                                type_: NumericLiteral,
                            },
                        ],
                    ),
                    SimpleAssignment(
                        "y",
                        [
                            Token {
                                span: LocatedSpan {
                                    offset: 28,
                                    line: 4,
                                    fragment: "20",
                                    extra: (),
                                },
                                type_: NumericLiteral,
                            },
                        ],
                    ),
                    Expression(
                        [
                            Token {
                                span: LocatedSpan {
                                    offset: 33,
                                    line: 5,
                                    fragment: "x",
                                    extra: (),
                                },
                                type_: Identifier,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 34,
                                    line: 5,
                                    fragment: " ",
                                    extra: (),
                                },
                                type_: Space,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 35,
                                    line: 5,
                                    fragment: "-",
                                    extra: (),
                                },
                                type_: PunctuationSoup,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 36,
                                    line: 5,
                                    fragment: " ",
                                    extra: (),
                                },
                                type_: Space,
                            },
                            Token {
                                span: LocatedSpan {
                                    offset: 37,
                                    line: 5,
                                    fragment: "y",
                                    extra: (),
                                },
                                type_: Identifier,
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
                                            Token {
                                                span: LocatedSpan {
                                                    offset: 27,
                                                    line: 5,
                                                    fragment: "10",
                                                    extra: (),
                                                },
                                                type_: NumericLiteral,
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
                    Token {
                        span: LocatedSpan {
                            offset: 4,
                            line: 1,
                            fragment: "=",
                            extra: (),
                        },
                        type_: EqualSign,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 5,
                            line: 1,
                            fragment: " ",
                            extra: (),
                        },
                        type_: Space,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 6,
                            line: 1,
                            fragment: "bar",
                            extra: (),
                        },
                        type_: Identifier,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 9,
                            line: 1,
                            fragment: " ",
                            extra: (),
                        },
                        type_: Space,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 10,
                            line: 1,
                            fragment: "=",
                            extra: (),
                        },
                        type_: EqualSign,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 11,
                            line: 1,
                            fragment: " ",
                            extra: (),
                        },
                        type_: Space,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 12,
                            line: 1,
                            fragment: "baz",
                            extra: (),
                        },
                        type_: Identifier,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 15,
                            line: 1,
                            fragment: "",
                            extra: (),
                        },
                        type_: Newline,
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
            r#"
Err(
    Error(
        Error {
            input: Tokens(
                [
                    Token {
                        span: LocatedSpan {
                            offset: 4,
                            line: 1,
                            fragment: "=",
                            extra: (),
                        },
                        type_: EqualSign,
                    },
                    Token {
                        span: LocatedSpan {
                            offset: 5,
                            line: 1,
                            fragment: "",
                            extra: (),
                        },
                        type_: Newline,
                    },
                ],
            ),
            code: Verify,
        },
    ),
)
"#
        );
    }
}
