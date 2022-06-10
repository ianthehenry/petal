mod coefficient_grouper;
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

use pos_parser::{ParseError, PartOfSpeech};
use terms::Term;

pub fn parse(input: &str) -> Result<(Term, PartOfSpeech), ParseError> {
    let tokens = tokenizer::tokenize(input);
    let terms = statement_parser::parse_expression(tokens).unwrap();
    let terms = semicolons::resolve_expression(terms);
    let terms = op_splitter::split_expression(terms);
    let terms = coefficient_grouper::group(terms);
    pos_parser::just_parse(terms)
}
