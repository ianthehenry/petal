mod coefficient_grouper;
mod helpers;
mod located_token;
mod location;
pub mod new_parser;
mod op_splitter;
pub mod pos_parser;
mod semicolons;
mod span;
mod statement;
mod terms;
pub mod token;
pub mod tokenizer;
mod tokens;

use pos_parser::{ParseError, PartOfSpeech};
use terms::Term;

pub fn parse(input: &str) -> Result<(Term, PartOfSpeech), ParseError> {
    let tokens = tokenizer::tokenize(input);
    let terms = new_parser::parse_expression(tokens).unwrap();
    let terms = semicolons::resolve_expression(terms);
    let terms = op_splitter::split_expression(terms);
    let terms = coefficient_grouper::group(terms);
    pos_parser::just_parse(terms)
}
