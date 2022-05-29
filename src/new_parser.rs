use core::fmt;
use std::num::NonZeroUsize;

use crate::helpers::*;
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{eof, map, map_opt, opt, verify},
    multi::{many0, many1},
    sequence::delimited,
    IResult, InputLength, InputTake,
};

use crate::new_tokenizer::{LocatedToken, Token};

type ParseResult<'a, R> = IResult<Tokens<'a>, R>;
type TokenResult<'a> = ParseResult<'a, &'a LocatedToken>;
type UnitResult<'a> = ParseResult<'a, ()>;

#[derive(Debug, Clone)]
struct Tokens<'a>(&'a [LocatedToken]);

impl<'a> Tokens<'a> {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

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

fn numeric_literal(i: Tokens) -> ParseResult<String> {
    map_opt(any_token, |t: &LocatedToken| match &t.token {
        Token::NumericLiteral(x) => Some(x.to_string()),
        _ => None,
    })(i)
}

fn punctuation_soup(i: Tokens) -> ParseResult<String> {
    map_opt(any_token, |t: &LocatedToken| match &t.token {
        Token::PunctuationSoup(x) => Some(x.to_string()),
        _ => None,
    })(i)
}

fn semicolons(i: Tokens) -> ParseResult<usize> {
    map_opt(any_token, |t: &LocatedToken| match &t.token {
        Token::Semicolons(x) => Some(*x),
        _ => None,
    })(i)
}

fn match_token<'a>(token: Token) -> impl FnMut(Tokens<'a>) -> TokenResult {
    verify(any_token, move |t: &LocatedToken| t.token == token)
}

fn skip_token<'a>(token: Token) -> impl FnMut(Tokens<'a>) -> UnitResult {
    ignore(match_token(token))
}

fn maybe_space(i: Tokens) -> UnitResult {
    ignore(opt(skip_token(Token::Space)))(i)
}

// We go through two passes. First we create terms, which do not know how to
// split sequences of punctuation characters into individual identifier tokens,
// and which preserve spaces. The next pass is a treewalk that uses currently
// defined operators to split punctuation soup into individual identifier
// tokens, and to disambiguate unary negation from subtraction.
#[derive(Debug, PartialEq, Clone)]
pub enum SoupyTerm {
    Identifier(String),
    PunctuationSoup(String),
    NumericLiteral(String),
    Parens(Vec<SoupyTerm>),
    Brackets(Vec<SoupyTerm>),
    Semicolons(usize),
    Space,
}

impl fmt::Display for SoupyTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn listed(terms: &[SoupyTerm], f: &mut fmt::Formatter) -> fmt::Result {
            let mut first = true;
            for term in terms {
                if first {
                    first = false;
                } else {
                    write!(f, " ")?;
                }
                write!(f, "{}", term)?;
            }
            Ok(())
        }

        use SoupyTerm::*;
        match self {
            Parens(terms) => {
                write!(f, "(")?;
                listed(terms, f)?;
                write!(f, ")")
            }
            Brackets(terms) => {
                write!(f, "[")?;
                listed(terms, f)?;
                write!(f, "]")
            }
            Space => write!(f, "␠"),
            Semicolons(count) => write!(f, "{}", ";".repeat(*count)),
            Identifier(s) | PunctuationSoup(s) | NumericLiteral(s) => write!(f, "{}", s),
        }
    }
}

// TODO: theoretically this should allow indent/outdent so that you could write
// expressions like:
//
//     x = (1 +
//          2)
//
// And replace newlines with spaces and stuff.
fn inner_expressions(i: Tokens) -> ParseResult<Expression<SoupyTerm>> {
    many0(term)(i)
}

fn term(i: Tokens) -> ParseResult<SoupyTerm> {
    alt((
        map(identifier, SoupyTerm::Identifier),
        map(numeric_literal, SoupyTerm::NumericLiteral),
        map(punctuation_soup, SoupyTerm::PunctuationSoup),
        map(semicolons, SoupyTerm::Semicolons),
        replace(match_token(Token::Space), SoupyTerm::Space),
        map(
            delimited(
                match_token(Token::OpenParen),
                inner_expressions,
                match_token(Token::CloseParen),
            ),
            SoupyTerm::Parens,
        ),
        map(
            delimited(
                match_token(Token::OpenBracket),
                inner_expressions,
                match_token(Token::CloseBracket),
            ),
            SoupyTerm::Brackets,
        ),
    ))(i)
}

fn expression(i: Tokens) -> ParseResult<Expression<SoupyTerm>> {
    many1(term)(i)
}

fn assignment_statement(i: Tokens) -> ParseResult<Statement<SoupyTerm>> {
    let (i, identifier) = identifier(i)?;
    let (i, ()) = maybe_space(i)?;
    let (i, ()) = skip_token(Token::EqualSign)(i)?;
    let (i, ()) = maybe_space(i)?;
    let (i, expression) = opt(expression)(i)?;
    let (i, ()) = skip_token(Token::Newline)(i)?;

    if let Ok((i, ())) = skip_token(Token::Indent)(i.clone()) {
        let (i, mut block) = statements(i)?;
        if let Some(expression) = expression {
            block.insert(0, Statement::Expression(expression));
        };
        Ok((i, Statement::CompoundAssignment(identifier, block)))
    } else if let Some(expression) = expression {
        Ok((i, Statement::SimpleAssignment(identifier, expression)))
    } else {
        // TODO: should be a custom error type
        Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        )))
    }
}

fn expression_statement(i: Tokens) -> ParseResult<Statement<SoupyTerm>> {
    let (i, expression) = map(expression, Statement::Expression)(i)?;
    let (i, ()) = skip_token(Token::Newline)(i)?;
    Ok((i, expression))
}

fn statement(i: Tokens) -> ParseResult<Statement<SoupyTerm>> {
    alt((assignment_statement, expression_statement))(i)
}

// separated by newlines until either outdent or EOF reached
fn statements(i: Tokens) -> ParseResult<Vec<Statement<SoupyTerm>>> {
    let (i, statements) = many1(statement)(i)?;
    let (i, ()) = alt((skip_token(Token::Outdent), ignore(eof)))(i)?;
    Ok((i, statements))
}

pub fn parse_tokens(tokens: Vec<LocatedToken>) -> Result<Vec<Statement<SoupyTerm>>, String> {
    let tokens = Tokens(&tokens);

    match statements(tokens) {
        Ok((remaining, block)) => {
            if remaining.is_empty() {
                Ok(block)
            } else {
                Err(format!("parse was not total. remaining: {:?}", remaining))
            }
        }
        Err(e) => Err(format!("{}", e)),
    }
}

type Block<T> = Vec<Statement<T>>;
type Expression<T> = Vec<T>;

#[derive(Debug)]
pub enum Statement<T> {
    SimpleAssignment(String, Expression<T>),
    CompoundAssignment(String, Block<T>),
    Expression(Expression<T>),
}

#[cfg(test)]
mod tests {
    use crate::new_tokenizer::tokenize;

    use super::*;

    fn show_expression(expression: &Expression<SoupyTerm>) -> String {
        expression
            .iter()
            .map(|t: &SoupyTerm| format!("{}", t))
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn show_block(block: &Block<SoupyTerm>) -> String {
        block
            .iter()
            .map(show_statement)
            .collect::<Vec<_>>()
            .join("; ")
    }

    fn show_statement(statement: &Statement<SoupyTerm>) -> String {
        match statement {
            Statement::SimpleAssignment(id, expr) => format!("{}={}", id, show_expression(expr)),
            Statement::CompoundAssignment(id, block) => format!("{}={{{}}}", id, show_block(block)),
            Statement::Expression(expr) => show_expression(expr),
        }
    }

    fn test(input: &str) -> String {
        let tokens = tokenize(input);
        let tokens = Tokens(&tokens);

        match statements(tokens) {
            Ok((remaining, block)) => {
                assert!(remaining.is_empty(), "parse was not total");
                show_block(&block)
            }
            Err(e) => format!("{}", e),
        }
    }

    #[test]
    fn simple_assignment() {
        k9::snapshot!(test("foo = bar"), "foo=bar");
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
            "foo={x=10; x ␠ * ␠ 2}"
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
            "foo={x ␠ + ␠ y; x=10; y=20; x ␠ - ␠ y}"
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
            "foo={x={y={10}}}"
        );
    }

    #[test]
    fn parse_errors() {
        k9::snapshot!(
            test("foo = bar = baz"),
            r#"Parsing Error: Error { input: Tokens([LocatedToken { location: Location { offset: 4, line: 1 }, token: EqualSign }, LocatedToken { location: Location { offset: 5, line: 1 }, token: Space }, LocatedToken { location: Location { offset: 6, line: 1 }, token: Identifier("bar") }, LocatedToken { location: Location { offset: 9, line: 1 }, token: Space }, LocatedToken { location: Location { offset: 10, line: 1 }, token: EqualSign }, LocatedToken { location: Location { offset: 11, line: 1 }, token: Space }, LocatedToken { location: Location { offset: 12, line: 1 }, token: Identifier("baz") }, LocatedToken { location: Location { offset: 15, line: 1 }, token: Newline }]), code: Verify }"#
        );

        k9::snapshot!(
            test("foo ="),
            "Parsing Error: Error { input: Tokens([LocatedToken { location: Location { offset: 4, line: 1 }, token: EqualSign }, LocatedToken { location: Location { offset: 5, line: 1 }, token: Newline }]), code: Verify }"
        );
    }
}
