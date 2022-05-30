use crate::statement::*;
use crate::terms::{SouplessTerm, Term};

fn convert(t: SouplessTerm) -> Term {
    match t {
        SouplessTerm::Identifier(s) => Term::Identifier(s),
        SouplessTerm::Operator(s) => Term::Identifier(s),
        SouplessTerm::NumericLiteral(s) => Term::NumericLiteral(s),
        SouplessTerm::Parens(terms) => Term::Parens(group(terms)),
        SouplessTerm::Brackets(terms) => Term::Brackets(group(terms)),
        SouplessTerm::Space => panic!(),
        SouplessTerm::MinusOperator => Term::Identifier("-".to_string()),
    }
}

fn group(terms: Vec<SouplessTerm>) -> Vec<Term> {
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
                result.push(Term::Parens(vec![
                    Term::Identifier("neg".to_string()),
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
    use crate::terms::{SouplessTerm, Term};

    fn delimited(start: &str, terms: Vec<Term>, end: &str) -> String {
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

    fn show_term(term: Term) -> String {
        match term {
            Term::Identifier(id) => id,
            Term::NumericLiteral(id) => id,
            Term::Parens(terms) => delimited("(", terms, ")"),
            Term::Brackets(terms) => delimited("[", terms, "]"),
            Term::Semicolons(count) => ";".repeat(count),
        }
    }

    fn test(input: &str) -> String {
        let terms = input
            .chars()
            .map(|c| {
                if c == ' ' {
                    SouplessTerm::Space
                } else if c == '-' {
                    SouplessTerm::MinusOperator
                } else if c.is_alphabetic() {
                    SouplessTerm::Identifier(c.to_string())
                } else if c.is_numeric() {
                    SouplessTerm::NumericLiteral(c.to_string())
                } else {
                    SouplessTerm::Operator(c.to_string())
                }
            })
            .collect();
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
    }
}

pub(super) fn rewrite(block: Block<SouplessTerm>) -> Block<Term> {
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
