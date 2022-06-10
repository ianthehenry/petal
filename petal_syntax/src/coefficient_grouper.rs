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
        match (next, iterator.peek()) {
            (
                minus @ MinusOperator,
                Some(MinusOperator | Operator(_) | Space | NumericLiteral(_)) | None,
            ) => result.push(convert(minus)),
            (MinusOperator, Some(Identifier(_) | Parens(_) | Brackets(_))) => {
                result.push(SpacelessTerm::Parens(vec![
                    SpacelessTerm::Coefficient("-1".to_string()),
                    convert(iterator.next().unwrap()),
                ]))
            }
            (
                num @ NumericLiteral(_),
                Some(MinusOperator | Operator(_) | Space | NumericLiteral(_)) | None,
            ) => result.push(convert(num)),
            (NumericLiteral(c), Some(Identifier(_) | Parens(_) | Brackets(_))) => {
                result.push(SpacelessTerm::Parens(vec![
                    SpacelessTerm::Coefficient(c),
                    convert(iterator.next().unwrap()),
                ]))
            }
            (SouplessTerm::Space, _) => (),
            (term @ (Identifier(_) | Parens(_) | Brackets(_)), lookahead) => {
                result.push(convert(term));
                if let Some(SouplessTerm::MinusOperator) = lookahead {
                    result.push(convert(iterator.next().unwrap()))
                }
            }
            (op @ Operator(_), _) => result.push(convert(op)),
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
            SpacelessTerm::Coefficient(c) => format!("<scale {}>", c),
            SpacelessTerm::Parens(terms) => delimited("(", terms, ")"),
            SpacelessTerm::Brackets(terms) => delimited("[", terms, "]"),
        }
    }

    fn test(input: &str) -> String {
        let tokens = crate::tokenizer::tokenize(input);
        let terms = crate::statement_parser::parse_expression(tokens).unwrap();
        let terms = crate::semicolons::resolve_expression(terms);
        let terms = crate::op_splitter::split_expression(terms);
        delimited("", group(terms), "")
    }

    #[test]
    fn ambiguous_subtraction_numbers() {
        k9::snapshot!(test("1-2"), "1 - 2");
        k9::snapshot!(test("1 -2"), "1 -2");
        k9::snapshot!(test("1- 2"), "1 - 2");
        k9::snapshot!(test("1 - 2"), "1 - 2");

        k9::snapshot!(test("1--2"), "1 - -2");
        k9::snapshot!(test("1 --2"), "1 - -2");
        k9::snapshot!(test("1- -2"), "1 - -2");
        k9::snapshot!(test("1-- 2"), "1 - - 2");
        k9::snapshot!(test("1 -- 2"), "1 - - 2");
    }

    #[test]
    fn ambiguous_subtraction_identifiers() {
        k9::snapshot!(test("x-y"), "x - y");
        k9::snapshot!(test("x -y"), "x (<scale -1> y)");
        k9::snapshot!(test("x- y"), "x - y");
        k9::snapshot!(test("x - y"), "x - y");

        k9::snapshot!(test("x--y"), "x - (<scale -1> y)");
        k9::snapshot!(test("x --y"), "x - (<scale -1> y)");
        k9::snapshot!(test("x- -y"), "x - (<scale -1> y)");
        k9::snapshot!(test("x-- y"), "x - - y");
        k9::snapshot!(test("x -- y"), "x - - y");
    }

    #[test]
    fn ambiguous_subtraction_parens_and_brackets() {
        k9::snapshot!(test("(x)-(y)"), "(x) - (y)");
        k9::snapshot!(test("(x) -(y)"), "(x) (<scale -1> (y))");

        k9::snapshot!(test("[x]-[y]"), "[x] - [y]");
        k9::snapshot!(test("[x] -[y]"), "[x] (<scale -1> [y])");
    }

    #[test]
    fn test_minus() {
        k9::snapshot!(test("-x"), "(<scale -1> x)");
        k9::snapshot!(test("--x"), "- (<scale -1> x)");
        k9::snapshot!(test("- x"), "- x");
        k9::snapshot!(test("- -x"), "- (<scale -1> x)");
        k9::snapshot!(test("x-y"), "x - y");
        k9::snapshot!(test("-x-y"), "(<scale -1> x) (<scale -1> y)");
        k9::snapshot!(test("x--y"), "x - (<scale -1> y)");
        k9::snapshot!(test("-x--y"), "(<scale -1> x) - (<scale -1> y)");
        k9::snapshot!(test("--x--y"), "- (<scale -1> x) - (<scale -1> y)");
        k9::snapshot!(test("x*-y"), "x * (<scale -1> y)");
        k9::snapshot!(test("x-*y"), "x - * y");
        k9::snapshot!(test("x* -y"), "x * (<scale -1> y)");
        k9::snapshot!(test("x*(-y)"), "x * ((<scale -1> y))");
        k9::snapshot!(test("-(x)"), "(<scale -1> (x))");
        k9::snapshot!(test("-(-x)"), "(<scale -1> ((<scale -1> x)))");
    }

    #[test]
    fn test_coefficients() {
        k9::snapshot!(test("2x"), "(<scale 2> x)");
        k9::snapshot!(test("2 x"), "2 x");
        k9::snapshot!(test("2(3)"), "(<scale 2> (3))");
        k9::snapshot!(test("2 (3)"), "2 (3)");
        k9::snapshot!(test("2(3)4"), "(<scale 2> (3)) 4");
        k9::snapshot!(test("2(3)4(5)"), "(<scale 2> (3)) (<scale 4> (5))");
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
