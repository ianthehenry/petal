use super::tokenizer;
use tokenizer::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Word {
    Int64(i64),
    Parens(Vec<Word>),
    Brackets(Vec<Word>),
    Identifier(String),
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
                "+" => Verb(Arity::Binary),
                "neg" => Verb(Arity::Unary),
                "." => Adverb(Arity::Binary, Arity::Binary),
                "fold" => Adverb(Arity::Unary, Arity::Unary),
                _ => Noun,
            },
            Word::Parens(words) => Self::of_words(words),
            Word::Brackets(_) => Noun,
        }
    }

    // a term is a noun or a verb
    fn pop_term(
        stack: &mut Vec<(Word, PartOfSpeech)>,
    ) -> Result<(Word, TermPartOfSpeech), TermPopError> {
        let result = stack.pop().ok_or(TermPopError::EndOfInput)?;
        match result {
            (word, PartOfSpeech::Noun) => Ok((word, TermPartOfSpeech::Noun)),
            (word, PartOfSpeech::Verb(arity)) => Ok((word, TermPartOfSpeech::Verb(arity))),
            (adverb, PartOfSpeech::Adverb(Arity::Unary, verb_arity)) => {
                // TODO: maybe better error handling here
                let (object, _) = Self::pop_term(stack)?;
                Ok((
                    // TODO: this should probably be a different "kind" of term
                    Word::Parens(vec![adverb, object]),
                    TermPartOfSpeech::Verb(verb_arity),
                ))
            }
            (_, PartOfSpeech::Adverb(Arity::Binary, _)) => Err(TermPopError::Conjunction),
        }
    }

    fn parse_terms(stack: &mut Vec<(Word, PartOfSpeech)>) -> Vec<(Word, TermPartOfSpeech)> {
        let mut terms = vec![];

        while !stack.is_empty() {
            if let (conjunction, PartOfSpeech::Adverb(Arity::Binary, verb_arity)) =
                stack.last().cloned().expect("empty stack")
            {
                stack.pop();
                let lhs = terms.pop();
                let rhs = Self::pop_term(stack);
                match (lhs, rhs) {
                    (None, _) | (_, Err(TermPopError::EndOfInput)) => {
                        panic!("parse error: partial conjunction application not yet supported")
                    }
                    (_, Err(TermPopError::Conjunction)) => {
                        panic!("parse error: conjunction conjunction")
                    }
                    // TODO: look at the input parts of speech to determine the
                    // output part of speech
                    (Some((lhs, _)), Ok((rhs, _))) => terms.push((
                        Word::Parens(vec![conjunction, lhs, rhs]),
                        TermPartOfSpeech::Verb(verb_arity),
                    )),
                }
            } else {
                terms.push(Self::pop_term(stack).expect("something bad"));
            }
        }
        terms
    }

    fn try_apply_suspended_conjunction(
        term_stack: &mut Vec<(Word, TermPartOfSpeech)>,
        suspended_conjunction: &mut Option<((Word, Arity), (Word, TermPartOfSpeech))>,
    ) {
        match suspended_conjunction {
            None => (),
            Some(((conjunction, out_arity), (rhs, _rhs_pos))) => {
                if let Some((lhs, _lhs_pos)) = term_stack.pop() {
                    term_stack.push((
                        Word::Parens(vec![conjunction.clone(), lhs, rhs.clone()]),
                        TermPartOfSpeech::Verb(out_arity.clone()),
                    ));
                    *suspended_conjunction = None;
                }
            }
        }
    }

    // this one doesn't work because it's overeager in applying conjunctions to
    // things to the right.
    fn stack_parser(stack: &mut Vec<(Word, PartOfSpeech)>) -> Vec<(Word, TermPartOfSpeech)> {
        let mut term_stack = vec![];
        let mut suspended_conjunction: Option<((Word, Arity), (Word, TermPartOfSpeech))> = None;

        while !stack.is_empty() {
            let next = stack.pop().unwrap();
            match &next {
                (word, PartOfSpeech::Noun) => {
                    Self::try_apply_suspended_conjunction(
                        &mut term_stack,
                        &mut suspended_conjunction,
                    );
                    term_stack.push((word.clone(), TermPartOfSpeech::Noun))
                }
                (word, PartOfSpeech::Verb(arity)) => {
                    Self::try_apply_suspended_conjunction(
                        &mut term_stack,
                        &mut suspended_conjunction,
                    );
                    term_stack.push((word.clone(), TermPartOfSpeech::Verb(arity.clone())))
                }
                (adverb, PartOfSpeech::Adverb(Arity::Unary, out_arity)) => {
                    let (arg, _arg_pos) = term_stack
                        .pop()
                        .expect("adverb has nothing to apply itself to");

                    term_stack.push((
                        Word::Parens(vec![adverb.clone(), arg]),
                        TermPartOfSpeech::Verb(out_arity.clone()),
                    ))
                }
                (conjunction, PartOfSpeech::Adverb(Arity::Binary, out_arity)) => {
                    match suspended_conjunction {
                        None => {
                            let rhs = term_stack.pop().expect("conjunction has no rhs");
                            suspended_conjunction =
                                Some(((conjunction.clone(), out_arity.clone()), rhs));
                        }
                        Some(((conjunction, out_arity), (rhs, _rhs_pos))) => {
                            let (lhs, _lhs_pos) =
                                term_stack.pop().expect("conjunction has no lhs!");

                            term_stack.push((
                                Word::Parens(vec![conjunction, lhs, rhs]),
                                TermPartOfSpeech::Verb(out_arity),
                            ));
                            suspended_conjunction = None;

                            stack.push(next);
                        }
                    }
                }
            }
        }

        // TODO: copy-paste
        if let Some(((conjunction, out_arity), (rhs, _rhs_pos))) = suspended_conjunction {
            let (lhs, _lhs_pos) = term_stack
                .pop()
                .expect("empty stack while applying suspended conjunction");

            term_stack.push((
                Word::Parens(vec![conjunction, lhs, rhs]),
                TermPartOfSpeech::Verb(out_arity),
            ));
        }

        term_stack
    }

    fn double_stack_parser(stack: &mut Vec<(Word, PartOfSpeech)>) -> Vec<(Word, TermPartOfSpeech)> {
        let mut term_stack = vec![];

        while !stack.is_empty() {
            let next = stack.pop().unwrap();
            match &next {
                (word, PartOfSpeech::Noun) => {
                    term_stack.push((word.clone(), TermPartOfSpeech::Noun))
                }
                (word, PartOfSpeech::Verb(arity)) => {
                    term_stack.push((word.clone(), TermPartOfSpeech::Verb(arity.clone())))
                }
                (adverb, PartOfSpeech::Adverb(Arity::Unary, out_arity)) => {
                    let (arg, _arg_pos) = term_stack
                        .pop()
                        .expect("adverb has nothing to apply itself to");

                    term_stack.push((
                        Word::Parens(vec![adverb.clone(), arg]),
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
                        Word::Parens(vec![conjunction.clone(), lhs, rhs]),
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

#[cfg(test)]
mod tests {
    use super::*;

    impl Word {
        fn delimited(start: &str, words: Vec<Word>, end: &str) -> String {
            let mut result = start.to_string();
            result += &words
                .into_iter()
                .map(|word| word.to_short_string())
                .collect::<Vec<_>>()
                .join(" ");
            result += end;
            result
        }

        fn to_short_string(self) -> String {
            match self {
                Word::Int64(num) => num.to_string(),
                Word::Identifier(id) => id.to_string(),
                Word::Parens(words) => Word::delimited("(", words, ")"),
                Word::Brackets(words) => Word::delimited("[", words, "]"),
            }
        }
    }

    fn test(input: &str) -> String {
        let (remaining, parsed) = tokenizer::tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        Word::delimited("", resolve_semicolons(parsed, Delimiter::Parens), "")
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

    #[test]
    fn adverbs() {
        let input = "x fold + . fold * y";
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
        annotated_words.reverse();

        let terms = PartOfSpeech::parse_terms(&mut annotated_words);
        k9::snapshot!(
            terms,
            r#"
[
    (
        Identifier(
            "x",
        ),
        Noun,
    ),
    (
        Parens(
            [
                Identifier(
                    ".",
                ),
                Parens(
                    [
                        Identifier(
                            "fold",
                        ),
                        Identifier(
                            "+",
                        ),
                    ],
                ),
                Parens(
                    [
                        Identifier(
                            "fold",
                        ),
                        Identifier(
                            "*",
                        ),
                    ],
                ),
            ],
        ),
        Verb(
            Binary,
        ),
    ),
    (
        Identifier(
            "y",
        ),
        Noun,
    ),
]
"#
        );
    }
    #[test]
    fn stack_parser() {
        let input = "x fold + . fold * y";
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

        let mut terms = PartOfSpeech::stack_parser(&mut annotated_words);
        terms.reverse();
        k9::snapshot!(
            terms,
            r#"
[
    (
        Identifier(
            "x",
        ),
        Noun,
    ),
    (
        Parens(
            [
                Identifier(
                    "fold",
                ),
                Identifier(
                    "+",
                ),
            ],
        ),
        Verb(
            Unary,
        ),
    ),
    (
        Parens(
            [
                Identifier(
                    ".",
                ),
                Identifier(
                    "y",
                ),
                Parens(
                    [
                        Identifier(
                            "fold",
                        ),
                        Identifier(
                            "*",
                        ),
                    ],
                ),
            ],
        ),
        Verb(
            Binary,
        ),
    ),
]
"#
        );
    }

    #[test]
    fn double_stack_parser() {
        let input = "x fold + . fold * y";
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

        let mut terms = PartOfSpeech::double_stack_parser(&mut annotated_words);
        terms.reverse();
        k9::snapshot!(
            terms,
            r#"
[
    (
        Identifier(
            "x",
        ),
        Noun,
    ),
    (
        Parens(
            [
                Identifier(
                    ".",
                ),
                Parens(
                    [
                        Identifier(
                            "fold",
                        ),
                        Identifier(
                            "+",
                        ),
                    ],
                ),
                Parens(
                    [
                        Identifier(
                            "fold",
                        ),
                        Identifier(
                            "*",
                        ),
                    ],
                ),
            ],
        ),
        Verb(
            Binary,
        ),
    ),
    (
        Identifier(
            "y",
        ),
        Noun,
    ),
]
"#
        );
    }
}
