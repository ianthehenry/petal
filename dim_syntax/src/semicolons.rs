use crate::statement::*;
use crate::terms::{SemiSoupyTerm, SoupyTerm};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Delimiter {
    Parens,
    Brackets,
}

fn wrap(terms: Vec<SoupyTerm>, delimiter: &Delimiter) -> SoupyTerm {
    match delimiter {
        Delimiter::Parens => SoupyTerm::Parens(terms),
        Delimiter::Brackets => SoupyTerm::Brackets(terms),
    }
}

fn resolve_semicolons(terms: Vec<SemiSoupyTerm>, delimiter: Delimiter) -> Vec<SoupyTerm> {
    let mut index_levels: Vec<usize> = vec![];
    let mut result: Vec<SoupyTerm> = vec![];
    for term in terms {
        match term {
            SemiSoupyTerm::Space => result.push(SoupyTerm::Space),
            SemiSoupyTerm::Identifier(s) => result.push(SoupyTerm::Identifier(s)),
            SemiSoupyTerm::NumericLiteral(s) => result.push(SoupyTerm::NumericLiteral(s)),
            SemiSoupyTerm::PunctuationSoup(s) => result.push(SoupyTerm::PunctuationSoup(s)),
            SemiSoupyTerm::Parens(terms) => result.push(SoupyTerm::Parens(resolve_semicolons(
                terms,
                Delimiter::Parens,
            ))),
            SemiSoupyTerm::Brackets(terms) => result.push(SoupyTerm::Brackets(resolve_semicolons(
                terms,
                Delimiter::Brackets,
            ))),
            SemiSoupyTerm::Semicolons(level) => {
                if index_levels.len() < level {
                    index_levels.resize(level, 0)
                }
                let next_index = index_levels[level - 1] + 1;

                for start_index in index_levels.iter_mut().take(level) {
                    let to_wrap = result.drain(*start_index..).collect();
                    result.push(wrap(to_wrap, &delimiter));
                    *start_index = next_index;
                }
            }
        }
    }

    for start_index in index_levels {
        let to_wrap = result.drain(start_index..).collect();
        result.push(wrap(to_wrap, &delimiter));
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(input: &str) -> String {
        let tokens = crate::new_tokenizer::tokenize(input);
        let terms = crate::new_parser::parse_expression(tokens).unwrap();
        resolve_semicolons(terms, Delimiter::Parens)
            .iter()
            .map(|x| format!("{}", x))
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn semicolon_resolution() {
        k9::snapshot!(test("[1]"), "[1]");
        k9::snapshot!(test("[1 2]"), "[1 ␠ 2]");
        k9::snapshot!(test("[1 2; 3 4]"), "[[1 ␠ 2] [␠ 3 ␠ 4]]");
        k9::snapshot!(test("[1 ;; 2]"), "[[[1 ␠]] [[␠ 2]]]");
        k9::snapshot!(test("[1 ; ;; 2]"), "[[[1 ␠] [␠]] [[␠ 2]]]");
        k9::snapshot!(test("[1 2; 3 4; 5 6]"), "[[1 ␠ 2] [␠ 3 ␠ 4] [␠ 5 ␠ 6]]");
        k9::snapshot!(
            test("[1 2; 3 4;; 5 6; 7 8]"),
            "[[[1 ␠ 2] [␠ 3 ␠ 4]] [[␠ 5 ␠ 6] [␠ 7 ␠ 8]]]"
        );

        k9::snapshot!(
            test("[1 2;; 3 4; 5 6]"),
            "[[[1 ␠ 2]] [[␠ 3 ␠ 4] [␠ 5 ␠ 6]]]"
        );
    }

    #[test]
    fn semicolons_in_parens() {
        k9::snapshot!(test("(1 + 2; f 3)"), "((1 ␠ + ␠ 2) (␠ f ␠ 3))");
        k9::snapshot!(test("(1 2; 3 4;; 5)"), "(((1 ␠ 2) (␠ 3 ␠ 4)) ((␠ 5)))");
    }

    #[test]
    fn nested_heterogeneous_semicolons() {
        k9::snapshot!(test("[[1; 2] [3 4]]"), "[[[1] [␠ 2]] ␠ [3 ␠ 4]]");
        k9::snapshot!(
            test("(1 2 (3 4; 5 6);; [7; 8])"),
            "(((1 ␠ 2 ␠ ((3 ␠ 4) (␠ 5 ␠ 6)))) ((␠ [[7] [␠ 8]])))"
        );
    }
}

pub(super) fn rewrite(block: Block<SemiSoupyTerm>) -> Block<SoupyTerm> {
    use Statement::*;
    block
        .into_iter()
        .map(|statement: Statement<SemiSoupyTerm>| match statement {
            SimpleAssignment(id, terms) => {
                SimpleAssignment(id, resolve_semicolons(terms, Delimiter::Parens))
            }
            CompoundAssignment(id, block) => CompoundAssignment(id, rewrite(block)),
            Expression(terms) => Expression(resolve_semicolons(terms, Delimiter::Parens)),
        })
        .collect()
}
