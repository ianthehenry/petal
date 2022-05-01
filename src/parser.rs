use super::tokenizer;
use std::fmt;
use tokenizer::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Word {
    Int64(i64),
    Parens(Vec<Word>),
    Brackets(Vec<Word>),
    Identifier(String),
}

impl Word {
    fn delimited(start: &str, words: &Vec<Word>, end: &str) -> String {
        let mut result = start.to_string();
        result += &words
            .iter()
            .map(|word| word.to_short_string())
            .collect::<Vec<_>>()
            .join(" ");
        result += end;
        result
    }

    fn to_short_string(&self) -> String {
        match self {
            Word::Int64(num) => num.to_string(),
            Word::Identifier(id) => id.to_string(),
            Word::Parens(words) => Word::delimited("(", words, ")"),
            Word::Brackets(words) => Word::delimited("[", words, "]"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Term {
    Atom(Word),
    Parens(Box<Term>),
    Brackets(Box<Term>),
    Tuple(Box<Term>, Box<Term>),
    FunctionApplication(Box<Term>, Box<Term>),
    OperatorApplication(Box<Term>, Box<Term>, Box<Term>),
    AdverbApplication(Box<Term>, Box<Term>),
    ConjunctionApplication(Box<Term>, Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
        match self {
            Atom(word) => write!(f, "{}", word.to_short_string()),
            Parens(term) => write!(f, "({})", term),
            Brackets(term) => write!(f, "[{}]", term),
            Tuple(fst, snd) => {
                write!(f, "(tuple {} {})", fst, snd)
            }
            AdverbApplication(adverb, term) => {
                write!(f, "(! {} {})", adverb, term)
            }
            ConjunctionApplication(conjunction, lhs, rhs) => {
                write!(f, "(! {} {} {})", conjunction, lhs, rhs)
            }
            FunctionApplication(function, term) => {
                write!(f, "({} {})", function, term)
            }
            OperatorApplication(operator, lhs, rhs) => {
                write!(f, "({} {} {})", operator, lhs, rhs)
            }
        }
    }
}

fn show_annotated_term(annotated_term: &(Term, PartOfSpeech)) -> String {
    let (term, pos) = annotated_term;
    format!("{}:{}", pos, term)
}

fn show_annotated_terms(terms: Vec<(Term, PartOfSpeech)>) -> String {
    terms
        .iter()
        .map(show_annotated_term)
        .collect::<Vec<_>>()
        .join(" ")
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Delimiter {
    Parens,
    Brackets,
}

impl Delimiter {
    fn wrap(&self, words: Vec<Word>) -> Word {
        match self {
            Self::Parens => Word::Parens(words),
            Self::Brackets => Word::Brackets(words),
        }
    }
}

fn resolve_semicolons(tokens: Vec<Token>, delimiter: Delimiter) -> Vec<Word> {
    let mut index_levels: Vec<usize> = vec![];
    let mut words: Vec<Word> = vec![];
    for token in tokens {
        match token {
            Token::Int64(value) => words.push(Word::Int64(value)),
            Token::Parens(tokens) => {
                words.push(Word::Parens(resolve_semicolons(tokens, Delimiter::Parens)))
            }
            Token::Brackets(tokens) => words.push(Word::Brackets(resolve_semicolons(
                tokens,
                Delimiter::Brackets,
            ))),
            Token::Identifier(value) => words.push(Word::Identifier(value)),
            Token::Semicolons(level) => {
                if index_levels.len() < level {
                    index_levels.resize(level, 0)
                }
                let next_index = index_levels[level - 1] + 1;

                for i in 0..level {
                    let start_index = index_levels[i];
                    let to_wrap = words.drain(start_index..).collect();
                    words.push(delimiter.wrap(to_wrap));
                    index_levels[i] = next_index;
                }
            }
        }
    }

    for start_index in index_levels {
        let to_wrap = words.drain(start_index..).collect();
        words.push(delimiter.wrap(to_wrap));
    }
    words
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Arity {
    Unary,
    Binary,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum PartOfSpeech {
    Noun,
    Verb(Arity),
    Adverb(Arity, Arity), // input arity, output arity
}

impl fmt::Display for PartOfSpeech {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PartOfSpeech::*;
        match self {
            Noun => write!(f, "n"),
            Verb(Arity::Unary) => write!(f, "f"),
            Verb(Arity::Binary) => write!(f, "o"),
            Adverb(Arity::Unary, _) => write!(f, "a"),
            Adverb(Arity::Binary, _) => write!(f, "c"),
        }
    }
}

fn parse_word(word: Word) -> (Term, PartOfSpeech) {
    use PartOfSpeech::*;

    match word {
        Word::Int64(_) => (Term::Atom(word), Noun),
        Word::Identifier(id) => {
            let pos = match id.as_str() {
                "+" | "*" => Verb(Arity::Binary),
                "neg" => Verb(Arity::Unary),
                "." => Adverb(Arity::Binary, Arity::Binary),
                "fold" => Adverb(Arity::Unary, Arity::Unary),
                _ => Noun,
            };
            // TODO: how can i borrow id here?
            (Term::Atom(Word::Identifier(id)), pos)
        }
        Word::Parens(mut words) => {
            let terms = table_parser(&mut words);
            if terms.len() == 1 {
                let (term, pos) = terms.into_iter().next().unwrap();
                (Term::Parens(Box::new(term)), pos)
            } else {
                panic!("failed to parse parenthesized expression")
            }
        }
        Word::Brackets(mut words) => {
            let terms = table_parser(&mut words);
            if terms.len() == 1 {
                let (term, pos) = terms.into_iter().next().unwrap();
                if pos != Noun {
                    panic!("bracketed array literals can only contain nouns")
                }
                (Term::Brackets(Box::new(term)), pos)
            } else {
                panic!("failed to parse bracketed array literal")
            }
        }
    }
}

fn table_parser(input: &mut Vec<Word>) -> Vec<(Term, PartOfSpeech)> {
    use Arity::*;
    use PartOfSpeech::*;
    use Term::*;
    let mut end_reached = false;
    let mut stack: Vec<Option<(Term, PartOfSpeech)>> = vec![None, None, None, None];
    loop {
        // These patterns are sort of written backwards from how I want to think
        // of them. We're walking right-to-left through the input tokens:
        //
        //    1 twice + 2
        //
        // So the stack will contain a reverse prefix from the right:
        //
        // 2
        // 2 +
        // 2 + twice
        // 2 (+ twice)
        // 2 (+ twice) 1
        match &stack[stack.len() - 4..] {
            [.., Some((_rhs, Adverb(Unary, _))), Some((_lhs, Adverb(Unary, _)))] => {
                panic!("adverb precomp")
            }
            [.., Some((verb, Verb(_))), Some((adverb, Adverb(Unary, result_arity)))] => {
                let result_arity = result_arity.clone();
                // TODO: we can pretty easily avoid cloning the terms. slash,
                // like, this entire situation can be greatly simplified by a
                // typed stack helper thing
                let adverb = adverb.clone();
                let verb = verb.clone();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    AdverbApplication(Box::new(adverb), Box::new(verb)),
                    Verb(result_arity),
                )));
            }
            [.., Some((noun, Noun)), Some((verb, Verb(Unary))), None | Some((_, Verb(_) | Noun))] =>
            {
                let noun = noun.clone();
                let verb = verb.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    FunctionApplication(Box::new(verb), Box::new(noun)),
                    Noun,
                )));
                stack.push(stash);
            }
            [.., Some((rhs, Noun | Verb(_))), Some((verb, Adverb(Binary, result_arity))), Some((lhs, Noun | Verb(_))), _] =>
            {
                let lhs = lhs.clone();
                let verb = verb.clone();
                let result_arity = result_arity.clone();
                let rhs = rhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    ConjunctionApplication(Box::new(verb), Box::new(lhs), Box::new(rhs)),
                    Verb(result_arity),
                )));
                stack.push(stash);
            }
            [.., Some((rhs, Noun)), Some((verb, Verb(Binary))), Some((lhs, Noun)), None | Some((_, Verb(_)))] =>
            {
                let lhs = lhs.clone();
                let verb = verb.clone();
                let rhs = rhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    OperatorApplication(Box::new(verb), Box::new(lhs), Box::new(rhs)),
                    Noun,
                )));
                stack.push(stash);
            }

            [.., Some((snd, Noun)), Some((fst, Noun)), None | Some((_, Verb(_) | Noun))] => {
                let first = fst.clone();
                let second = snd.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((Tuple(Box::new(first), Box::new(second)), Noun)));
                stack.push(stash);
            }

            _ => match input.pop() {
                None => {
                    if end_reached {
                        break;
                    } else {
                        end_reached = true;
                        stack.push(None);
                    }
                }
                Some(word) => {
                    stack.push(Some(parse_word(word)));
                }
            },
        };
    }
    stack.into_iter().flatten().collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(input: &str) -> String {
        let (remaining, parsed) = tokenizer::tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        Word::delimited("", &resolve_semicolons(parsed, Delimiter::Parens), "")
    }

    #[test]
    fn semicolon_resolution() {
        k9::snapshot!(test("[1]"), "[1]");
        k9::snapshot!(test("[1 2]"), "[1 2]");
        k9::snapshot!(test("[1 2; 3 4]"), "[[1 2] [3 4]]");
        k9::snapshot!(test("[1 ;; 2]"), "[[[1]] [[2]]]");
        k9::snapshot!(test("[1 ; ;; 2]"), "[[[1] []] [[2]]]");
        k9::snapshot!(test("[1 2; 3 4; 5 6]"), "[[1 2] [3 4] [5 6]]");
        k9::snapshot!(
            test("[1 2; 3 4;; 5 6; 7 8]"),
            "[[[1 2] [3 4]] [[5 6] [7 8]]]"
        );

        k9::snapshot!(test("[1 2;; 3 4; 5 6]"), "[[[1 2]] [[3 4] [5 6]]]");
    }

    #[test]
    fn semicolons_in_parens() {
        k9::snapshot!(test("(1 + 2; f 3)"), "((1 + 2) (f 3))");
        k9::snapshot!(test("(1 2; 3 4;; 5)"), "(((1 2) (3 4)) ((5)))");
    }

    #[test]
    fn nested_heterogeneous_semicolons() {
        k9::snapshot!(test("[[1; 2] [3 4]]"), "[[[1] [2]] [3 4]]");
        k9::snapshot!(
            test("(1 2 (3 4; 5 6);; [7; 8])"),
            "(((1 2 ((3 4) (5 6)))) (([[7] [8]])))"
        );
    }

    fn tester(input: &str) -> String {
        let (remaining, parsed) = tokenizer::tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        let mut words = resolve_semicolons(parsed, Delimiter::Parens);
        let terms = table_parser(&mut words);
        show_annotated_terms(terms)
    }

    #[test]
    fn test_table_parser() {
        k9::snapshot!(tester("neg 1 + 2"), "n:(neg (+ 1 2))");
        k9::snapshot!(tester("fold +"), "f:(! fold +)");
        k9::snapshot!(tester("fold + x"), "n:((! fold +) x)");
        k9::snapshot!(tester("x + y"), "n:(+ x y)");
        k9::snapshot!(tester("x +.* y"), "n:((! . + *) x y)");
        k9::snapshot!(tester("x fold + . * y"), "n:((! . (! fold +) *) x y)");
        k9::snapshot!(tester("x + . fold * y"), "n:((! . + (! fold *)) x y)");
        k9::snapshot!(
            tester("x fold + . fold * y"),
            "n:((! . (! fold +) (! fold *)) x y)"
        );
        k9::snapshot!(
            tester("x fold * . fold + . fold * y"),
            "n:((! . (! fold *) (! . (! fold +) (! fold *))) x y)"
        );
    }

    #[test]
    fn test_tuples() {
        k9::snapshot!(tester("1 + 1 2"), "n:(+ 1 (tuple 1 2))");
        k9::snapshot!(tester("1 + 1 neg 2"), "n:(+ 1 (tuple 1 (neg 2)))");
        k9::snapshot!(tester("1 2 + 1 2"), "n:(+ (tuple 1 2) (tuple 1 2))");
        k9::snapshot!(tester("1 + (1 2) 3"), "n:(+ 1 (tuple ((tuple 1 2)) 3))");
        k9::snapshot!(tester("1 + (1 2 3)"), "n:(+ 1 ((tuple 1 (tuple 2 3))))");
        k9::snapshot!(tester("1 + 2; 3"), "n:(tuple ((+ 1 2)) (3))");
    }
}
