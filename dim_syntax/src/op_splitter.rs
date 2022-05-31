use std::cmp::Reverse;

use crate::statement::*;
use crate::terms::{SouplessTerm, SoupyTerm};

// So yeah okay. Here's what we're gonna do. we're only going to keep track of
// multi-character tokens.
fn split_tokens(mut input: &str, scope: &Scope) -> Vec<SouplessTerm> {
    let mut result = Vec::new();

    // TODO: this is a little dumb. the scope should know *all* defined
    // operators -- it shouldn't fallback to splitting on single characters. it
    // would probably be nicer to get an error here.
    while !input.is_empty() {
        if let Some((op, remaining)) = scope.strip_prefix(input) {
            if op == "-" {
                result.push(SouplessTerm::MinusOperator);
            } else {
                result.push(SouplessTerm::Operator(op.to_string()));
            }
            input = remaining;
        } else {
            let op = &input[0..1];
            if op == "-" {
                result.push(SouplessTerm::MinusOperator);
            } else {
                result.push(SouplessTerm::Operator(op.to_string()));
            }
            input = &input[1..];
        }
    }
    result
}

#[test]
fn test_split_tokens() {
    fn test(input: &str, ops: &[&str]) -> Vec<SouplessTerm> {
        let mut scope = Scope::new(None);
        for op in ops {
            scope.learn(op);
        }
        split_tokens(input, &scope)
    }

    k9::snapshot!(
        test("+-+", &[]),
        r#"[Operator("+"), MinusOperator, Operator("+")]"#
    );
    k9::snapshot!(test("+-+", &["+-"]), r#"[Operator("+-"), Operator("+")]"#);
    k9::snapshot!(test("+-+", &["-+"]), r#"[Operator("+"), Operator("-+")]"#);
    k9::snapshot!(
        test("+-+", &["-+", "+-"]),
        r#"[Operator("+-"), Operator("+")]"#
    );
}

fn get_initial_scope<'a>() -> Scope<'a> {
    let mut scope = Scope::new(None);
    // TODO: fill out all the built-in operators here
    scope.learn(">=");
    scope.learn("<=");
    scope
}

fn split(terms: Vec<SoupyTerm>, scope: &Scope) -> Vec<SouplessTerm> {
    terms
        .into_iter()
        .flat_map(|term: SoupyTerm| match term {
            SoupyTerm::Identifier(s) => vec![SouplessTerm::Identifier(s)],
            SoupyTerm::PunctuationSoup(s) => split_tokens(&s, scope),
            SoupyTerm::NumericLiteral(s) => vec![SouplessTerm::NumericLiteral(s)],
            SoupyTerm::Parens(terms) => vec![SouplessTerm::Parens(split(terms, scope))],
            SoupyTerm::Brackets(terms) => vec![SouplessTerm::Brackets(split(terms, scope))],
            SoupyTerm::Space => vec![SouplessTerm::Space],
        })
        .collect()
}

struct Scope<'a> {
    operators: Vec<String>,
    parent: Option<&'a Scope<'a>>,
}

// TODO: convert this to a trie. horribly inefficient searching and adding
impl<'a> Scope<'a> {
    fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Scope {
            operators: vec![],
            parent,
        }
    }

    fn learn(&mut self, op: &str) {
        if op.len() > 1 {
            self.operators.push(op.to_string())
        }
        // TODO: yes i am aware how terrible this is
        self.operators.sort_by_key(|s| Reverse(s.len()));
    }

    fn strip_my_prefix<'s>(&self, input: &'s str) -> Option<(&str, &'s str)> {
        for known_token in &self.operators {
            if let Some(remaining) = input.strip_prefix(known_token) {
                return Some((known_token, remaining));
            }
        }
        None
    }

    fn strip_prefix<'s>(&self, input: &'s str) -> Option<(&str, &'s str)> {
        let mine = self.strip_my_prefix(input);
        let parent = match self.parent {
            None => None,
            Some(parent) => parent.strip_prefix(input),
        };
        match (mine, parent) {
            (Some((op1, _)), Some((op2, _))) => {
                if op1.len() > op2.len() {
                    mine
                } else {
                    parent
                }
            }
            (Some(_), None) => mine,
            (None, Some(_)) => parent,
            (None, None) => None,
        }
    }
}

fn rewrite_block(block: Block<SoupyTerm>, parent_scope: &Scope) -> Block<SouplessTerm> {
    use Statement::*;
    let mut scope = Scope::new(Some(parent_scope));
    block
        .into_iter()
        .map(|statement: Statement<SoupyTerm>| match statement {
            SimpleAssignment(id, terms) => {
                scope.learn(&id);
                SimpleAssignment(id, split(terms, &scope))
            }
            CompoundAssignment(id, block) => {
                scope.learn(&id);
                CompoundAssignment(id, rewrite_block(block, &scope))
            }
            Expression(terms) => Expression(split(terms, &scope)),
        })
        .collect()
}

#[cfg(test)]
pub(super) fn split_expression(terms: Vec<SoupyTerm>) -> Vec<SouplessTerm> {
    let prelude = get_initial_scope();
    split(terms, &prelude)
}

pub(super) fn rewrite(block: Block<SoupyTerm>) -> Block<SouplessTerm> {
    let prelude = get_initial_scope();
    rewrite_block(block, &prelude)
}
