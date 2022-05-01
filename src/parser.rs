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
    Word(Word),
    AdverbApplication(Word, Box<Term>),
    ConjunctionApplication(Word, Box<Term>, Box<Term>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum AnyTerm {
    Placeholder,
    Word(Word),
    FunctionApplication(Box<AnyTerm>, Box<AnyTerm>),
    OperatorApplication(Box<AnyTerm>, Box<AnyTerm>, Box<AnyTerm>),
    AdverbApplication(Box<AnyTerm>, Box<AnyTerm>),
    ConjunctionApplication(Box<AnyTerm>, Box<AnyTerm>, Box<AnyTerm>),
}

impl fmt::Display for AnyTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use AnyTerm::*;
        match self {
            Placeholder => write!(f, "_"),
            Word(word) => write!(f, "{}", word.to_short_string()),
            AdverbApplication(adverb, term) => {
                write!(f, "(A {} {})", adverb, term)
            }
            ConjunctionApplication(conjunction, lhs, rhs) => {
                write!(f, "(C {} {} {})", conjunction, lhs, rhs)
            }
            FunctionApplication(function, term) => {
                write!(f, "(F {} {})", function, term)
            }
            OperatorApplication(operator, lhs, rhs) => {
                write!(f, "(O {} {} {})", operator, lhs, rhs)
            }
        }
    }
}

fn show_annotated_term(annotated_term: &(AnyTerm, PartOfSpeech)) -> String {
    let (term, pos) = annotated_term;
    format!("{}:{}", pos, term)
}

fn show_annotated_terms(terms: Vec<(AnyTerm, PartOfSpeech)>) -> String {
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum TermPartOfSpeech {
    Noun,
    Verb(Arity),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum TermPopError {
    EndOfInput,
    Conjunction,
}

impl PartOfSpeech {
    fn of_word(word: &Word) -> Self {
        use PartOfSpeech::*;

        match word {
            Word::Int64(_) => Noun,
            Word::Identifier(id) => match id.as_str() {
                "+" | "*" => Verb(Arity::Binary),
                "neg" => Verb(Arity::Unary),
                "." => Adverb(Arity::Binary, Arity::Binary),
                "fold" => Adverb(Arity::Unary, Arity::Unary),
                _ => Noun,
            },
            Word::Parens(words) => Self::of_words(words),
            Word::Brackets(_) => Noun,
        }
    }

    fn double_stack_parser(stack: &mut Vec<(Word, PartOfSpeech)>) -> Vec<(Term, TermPartOfSpeech)> {
        let mut term_stack = vec![];

        while !stack.is_empty() {
            let next = stack.pop().unwrap();
            match &next {
                (word, PartOfSpeech::Noun) => {
                    term_stack.push((Term::Word(word.clone()), TermPartOfSpeech::Noun))
                }
                (word, PartOfSpeech::Verb(arity)) => term_stack.push((
                    Term::Word(word.clone()),
                    TermPartOfSpeech::Verb(arity.clone()),
                )),
                (adverb, PartOfSpeech::Adverb(Arity::Unary, out_arity)) => {
                    let (arg, _arg_pos) = term_stack
                        .pop()
                        .expect("adverb has nothing to apply itself to");

                    term_stack.push((
                        Term::AdverbApplication(adverb.clone(), Box::new(arg)),
                        TermPartOfSpeech::Verb(out_arity.clone()),
                    ))
                }
                (conjunction, PartOfSpeech::Adverb(Arity::Binary, out_arity)) => {
                    let (rhs, _rhs_pos) = term_stack.pop().expect("conjunction has no rhs");
                    let mut lefthand_terms = Self::double_stack_parser(stack);
                    // TODO: we just made ourselves unnecessarily quadratic.
                    // should return a deque or somthing instead.
                    lefthand_terms.reverse();
                    let (lhs, _lhs_pos) = lefthand_terms.pop().expect("conjunction has no lhs");
                    term_stack.push((
                        Term::ConjunctionApplication(
                            conjunction.clone(),
                            Box::new(lhs),
                            Box::new(rhs),
                        ),
                        TermPartOfSpeech::Verb(out_arity.clone()),
                    ));

                    while !lefthand_terms.is_empty() {
                        term_stack.push(lefthand_terms.pop().unwrap())
                    }
                }
            }
        }

        term_stack
    }

    // TODO: obviously this doesn't work at all
    fn of_annotated_words(annotated_words: &[(&Word, PartOfSpeech)]) -> Self {
        use PartOfSpeech::*;

        match annotated_words {
            [] => panic!("empty words!"),
            [(_, pos)] => *pos,
            [(_, Noun), ..] => Noun,
            [(_, Verb(arity)), ..] => Verb(*arity),
            [(_, Adverb(_, arity)), ..] => Verb(*arity),
            _ => Noun,
        }
    }

    fn of_words(words: &Vec<Word>) -> Self {
        let annotated_words = words
            .iter()
            .map(|x| (x, Self::of_word(x)))
            .collect::<Vec<_>>();
        Self::of_annotated_words(&annotated_words)
    }
}

fn table_parser(input: &mut Vec<(Word, PartOfSpeech)>) -> Vec<(AnyTerm, PartOfSpeech)> {
    use AnyTerm::*;
    use Arity::*;
    use PartOfSpeech::*;
    let mut end_reached = false;
    let mut stack: Vec<Option<(AnyTerm, PartOfSpeech)>> = vec![None, None, None, None];
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
            [.., Some((rhs, Noun)), Some((verb, Verb(Binary))), Some((lhs, Noun)), None | Some((_, Verb(_) | Noun))] =>
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

            _ => match input.pop() {
                None => {
                    if end_reached {
                        break;
                    } else {
                        end_reached = true;
                        stack.push(None);
                    }
                }
                Some((word, pos)) => stack.push(Some((Word(word), pos))),
            },
        };
        println!("{:?}", &stack);
    }
    stack.into_iter().flatten().collect::<Vec<_>>()
}

fn tester(input: &str) -> String {
    let (remaining, parsed) = tokenizer::tokens(input).unwrap();
    if !remaining.is_empty() {
        panic!("not a total parse!");
    }
    let words = resolve_semicolons(parsed, Delimiter::Parens);
    let mut annotated_words = words
        .into_iter()
        .map(|x| {
            let part_of_speech = PartOfSpeech::of_word(&x);
            (x, part_of_speech)
        })
        .collect::<Vec<_>>();

    let mut terms = table_parser(&mut annotated_words);
    show_annotated_terms(terms)
}

#[test]
fn test_table_parser() {
    k9::snapshot!(tester("neg 1 + 2"), "n:(F neg (O + 1 2))");
    k9::snapshot!(tester("fold +"), "f:(A fold +)");
    k9::snapshot!(tester("fold + x"), "n:(F (A fold +) x)");
    k9::snapshot!(tester("x + y"), "n:(O + x y)");
    k9::snapshot!(tester("x +.* y"), "n:(O (C . + *) x y)");
    k9::snapshot!(tester("x fold + . * y"), "n:(O (C . (A fold +) *) x y)");
    k9::snapshot!(tester("x + . fold * y"), "n:(O (C . + (A fold *)) x y)");
    k9::snapshot!(
        tester("x fold + . fold * y"),
        "n:(O (C . (A fold +) (A fold *)) x y)"
    );
    k9::snapshot!(
        tester("x fold * . fold + . fold * y"),
        "n:(O (C . (A fold *) (C . (A fold +) (A fold *))) x y)"
    );
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
}
