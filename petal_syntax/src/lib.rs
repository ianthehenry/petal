mod coefficient_grouper;
pub mod expression;
mod helpers;
mod located_token;
mod location;
mod op_splitter;
pub mod pos_parser;
mod semicolons;
mod span;
mod statement;
pub mod statement_parser;
mod terms;
pub mod token;
pub mod tokenizer;
mod tokens;

use expression::Expression;
use pos_parser::{ParseError, PartOfSpeech};

pub fn parse(input: &str) -> Result<(Expression, PartOfSpeech), ParseError> {
    let tokens = tokenizer::tokenize(input);
    let statements = statement_parser::parse_tokens(tokens).unwrap();
    let statements = semicolons::rewrite(statements);
    let statements = op_splitter::rewrite(statements);
    let statements = coefficient_grouper::rewrite(statements);
    pos_parser::just_parse(statements)
}
