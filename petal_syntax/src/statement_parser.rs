use crate::helpers::*;
use crate::located_token::*;
use crate::statement::*;
use crate::terms::SemiSoupyTerm;
use crate::token::*;
use crate::tokens::*;
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{eof, map, map_opt, opt, verify},
    multi::{many0, many1},
    sequence::delimited,
    IResult,
};

type ParseResult<'a, R> = IResult<Tokens<'a>, R>;
type TokenResult<'a> = ParseResult<'a, &'a LocatedToken>;
type UnitResult<'a> = ParseResult<'a, ()>;

fn any_token(i: Tokens) -> TokenResult {
    map(
        take(1usize),
        |tokens: Tokens| &tokens.0[0], // tokens.first()
    )(i)
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

// TODO: theoretically this should allow indent/outdent so that you could write
// expressions like:
//
//     x = (1 +
//          2)
//
// And replace newlines with spaces and stuff.
fn inner_expressions(i: Tokens) -> ParseResult<Terms<SemiSoupyTerm>> {
    many0(term)(i)
}

fn term(i: Tokens) -> ParseResult<SemiSoupyTerm> {
    alt((
        map(identifier, SemiSoupyTerm::Identifier),
        map(numeric_literal, SemiSoupyTerm::NumericLiteral),
        map(punctuation_soup, SemiSoupyTerm::PunctuationSoup),
        map(semicolons, SemiSoupyTerm::Semicolons),
        replace(match_token(Token::Space), SemiSoupyTerm::Space),
        map(
            delimited(
                match_token(Token::OpenParen),
                inner_expressions,
                match_token(Token::CloseParen),
            ),
            SemiSoupyTerm::Parens,
        ),
        map(
            delimited(
                match_token(Token::OpenBracket),
                inner_expressions,
                match_token(Token::CloseBracket),
            ),
            SemiSoupyTerm::Brackets,
        ),
    ))(i)
}

fn expression(i: Tokens) -> ParseResult<Terms<SemiSoupyTerm>> {
    many1(term)(i)
}

fn assignment_statement(i: Tokens) -> ParseResult<Statement<SemiSoupyTerm>> {
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

fn expression_statement(i: Tokens) -> ParseResult<Statement<SemiSoupyTerm>> {
    let (i, expression) = map(expression, Statement::Expression)(i)?;
    let (i, ()) = skip_token(Token::Newline)(i)?;
    Ok((i, expression))
}

fn statement(i: Tokens) -> ParseResult<Statement<SemiSoupyTerm>> {
    alt((assignment_statement, expression_statement))(i)
}

// separated by newlines until either outdent or EOF reached
fn statements(i: Tokens) -> ParseResult<Vec<Statement<SemiSoupyTerm>>> {
    let (i, statements) = many1(statement)(i)?;
    let (i, ()) = alt((skip_token(Token::Outdent), ignore(eof)))(i)?;
    Ok((i, statements))
}

// TODO: this should be test when i'm not using it in main anymore
// #[cfg(test)]
pub(super) fn parse_expression(tokens: Vec<LocatedToken>) -> Result<Terms<SemiSoupyTerm>, String> {
    let i = Tokens::new(&tokens);
    let (i, expression) = expression(i).map_err(|e| format!("{}", e))?;
    let (i, ()) = skip_token(Token::Newline)(i).map_err(|e| format!("{}", e))?;
    if i.is_empty() {
        Ok(expression)
    } else {
        Err(format!("parse was not total. remaining: {:?}", i))
    }
}

pub(super) fn parse_tokens(
    tokens: Vec<LocatedToken>,
) -> Result<Vec<Statement<SemiSoupyTerm>>, String> {
    let tokens = Tokens::new(&tokens);

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

#[cfg(test)]
mod tests {
    use crate::tokenizer::tokenize;

    use super::*;

    fn show_expression(expression: &Terms<SemiSoupyTerm>) -> String {
        expression
            .iter()
            .map(|t: &SemiSoupyTerm| format!("{}", t))
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn show_block(block: &Block<SemiSoupyTerm>) -> String {
        block
            .iter()
            .map(show_statement)
            .collect::<Vec<_>>()
            .join("; ")
    }

    fn show_statement(statement: &Statement<SemiSoupyTerm>) -> String {
        match statement {
            Statement::SimpleAssignment(id, expr) => format!("{}={}", id, show_expression(expr)),
            Statement::CompoundAssignment(id, block) => format!("{}={{{}}}", id, show_block(block)),
            Statement::Expression(expr) => show_expression(expr),
        }
    }

    fn test(input: &str) -> String {
        let tokens = tokenize(input);
        let tokens = Tokens::new(&tokens);

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
