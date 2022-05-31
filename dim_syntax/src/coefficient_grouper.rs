use crate::statement::*;
use crate::terms::{SouplessTerm, SpacelessTerm};

fn convert(t: SouplessTerm) -> SpacelessTerm {
    match t {
        SouplessTerm::Identifier(s) => SpacelessTerm::Identifier(s),
        SouplessTerm::Operator(s) => SpacelessTerm::Identifier(s),
        SouplessTerm::NumericLiteral(s) => SpacelessTerm::NumericLiteral(s),
        SouplessTerm::Parens(terms) => SpacelessTerm::Parens(group(terms)),
        SouplessTerm::Brackets(terms) => SpacelessTerm::Brackets(group(terms)),
        SouplessTerm::Space => panic!(),
        SouplessTerm::MinusOperator => SpacelessTerm::Identifier("-".to_string()),
    }
}

pub(super) fn group(terms: Vec<SouplessTerm>) -> Vec<SpacelessTerm> {
    let mut result = vec![];
    let mut iterator = terms.into_iter().peekable();
    use SouplessTerm::*;

    while let Some(next) = iterator.next() {
        match (&next, iterator.peek()) {
            (MinusOperator, Some(MinusOperator | Operator(_) | Space) | None) => {
                result.push(convert(next))
            }
            (MinusOperator, Some(NumericLiteral(_))) => {
                panic!("this should have parsed as a single thing")
            }
            (MinusOperator, Some(Identifier(_) | Parens(_) | Brackets(_))) => {
                result.push(SpacelessTerm::Parens(vec![
                    SpacelessTerm::Identifier("neg".to_string()),
                    convert(iterator.next().unwrap()),
                ]))
            }
            (SouplessTerm::Space, _) => (),
            (NumericLiteral(_) | Identifier(_) | Parens(_) | Brackets(_), lookahead) => {
                result.push(convert(next));
                if let Some(SouplessTerm::MinusOperator) = lookahead {
                    result.push(convert(iterator.next().unwrap()))
                }
            }
            (Operator(_), _) => result.push(convert(next)),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn delimited(start: &str, terms: Vec<SpacelessTerm>, end: &str) -> String {
        let mut result = start.to_string();
        let mut first = true;
        for term in terms {
            if first {
                first = false;
            } else {
                result += " ";
            }
            result += &show_term(term);
        }
        result += end;
        result
    }

    fn show_term(term: SpacelessTerm) -> String {
        match term {
            SpacelessTerm::Identifier(id) => id,
            SpacelessTerm::NumericLiteral(id) => id,
            SpacelessTerm::Parens(terms) => delimited("(", terms, ")"),
            SpacelessTerm::Brackets(terms) => delimited("[", terms, "]"),
        }
    }

    fn test(input: &str) -> String {
        let tokens = crate::tokenizer::tokenize(input);
        let terms = crate::new_parser::parse_expression(tokens).unwrap();
        let terms = crate::semicolons::resolve_expression(terms);
        let terms = crate::op_splitter::split_expression(terms);
        delimited("", group(terms), "")
    }

    #[test]
    fn test_group() {
        k9::snapshot!(test("-x"), "(neg x)");
        k9::snapshot!(test("--x"), "- (neg x)");
        k9::snapshot!(test("- x"), "- x");
        k9::snapshot!(test("- -x"), "- (neg x)");
        k9::snapshot!(test("x-y"), "x - y");
        k9::snapshot!(test("-x-y"), "(neg x) (neg y)");
        k9::snapshot!(test("x--y"), "x - (neg y)");
        k9::snapshot!(test("-x--y"), "(neg x) - (neg y)");
        k9::snapshot!(test("--x--y"), "- (neg x) - (neg y)");
        k9::snapshot!(test("x*-y"), "x * (neg y)");
        k9::snapshot!(test("x-*y"), "x - * y");
        k9::snapshot!(test("x* -y"), "x * (neg y)");
        k9::snapshot!(test("x*(-y)"), "x * ((neg y))");
        k9::snapshot!(test("-(x)"), "(neg (x))");
    }
}

pub(super) fn rewrite(block: Block<SouplessTerm>) -> Block<SpacelessTerm> {
    use Statement::*;
    block
        .into_iter()
        .map(|statement: Statement<SouplessTerm>| match statement {
            SimpleAssignment(id, terms) => SimpleAssignment(id, group(terms)),
            CompoundAssignment(id, block) => CompoundAssignment(id, rewrite(block)),
            Expression(terms) => Expression(group(terms)),
        })
        .collect()
}
