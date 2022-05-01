use super::tokenizer::Token;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Word {
    Int64(i64),
    Parens(Vec<Word>),
    Brackets(Vec<Word>),
    Identifier(String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Builtin {
    PartialApplicationLeft,
    PartialApplicationRight,
    Compose,
    ComposeLeft,
    ComposeRight,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Atom {
    Int64(i64),
    Identifier(String),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Int64(num) => write!(f, "{}", num),
            Atom::Identifier(id) => write!(f, "{}", id),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Implicit(Builtin),
    Atom(Atom),
    Parens(Box<Term>),
    // note: currently tuples and brackets store their elements in reverse order
    Tuple(Vec<Term>),
    Brackets(Vec<Term>),
    UnaryApplication(Box<Term>, Box<Term>),
    BinaryApplication(Box<Term>, Box<Term>, Box<Term>),
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Builtin::*;
        match self {
            PartialApplicationLeft => write!(f, "lhs"),
            PartialApplicationRight => write!(f, "rhs"),
            Compose => write!(f, "comp"),
            ComposeLeft => write!(f, "comp-lhs"),
            ComposeRight => write!(f, "comp-rhs"),
        }
    }
}
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
        match self {
            Atom(word) => write!(f, "{}", word),
            Implicit(builtin) => write!(f, "<{}>", builtin),
            Parens(term) => write!(f, "{}", term),
            Brackets(terms) => {
                write!(f, "[")?;
                for (i, term) in terms.iter().rev().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", term)?;
                }
                write!(f, "]")
            }
            Tuple(terms) => {
                write!(f, "(<tuple>")?;
                for term in terms.iter().rev() {
                    write!(f, " {}", term)?;
                }
                write!(f, ")")
            }
            UnaryApplication(func, term) => {
                write!(f, "({} {})", func, term)
            }
            BinaryApplication(func, lhs, rhs) => {
                write!(f, "({} {} {})", func, lhs, rhs)
            }
        }
    }
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

                for start_index in index_levels.iter_mut().take(level) {
                    let to_wrap = words.drain(*start_index..).collect();
                    words.push(delimiter.wrap(to_wrap));
                    *start_index = next_index;
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
pub enum Arity {
    Unary,
    Binary,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PartOfSpeech {
    Noun,
    Verb(Arity),
    Adverb(Arity, Arity), // input arity, output arity
}

impl fmt::Display for PartOfSpeech {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PartOfSpeech::*;
        match self {
            Noun => write!(f, "n"),
            Verb(Arity::Unary) => write!(f, "v1"),
            Verb(Arity::Binary) => write!(f, "v2"),
            Adverb(Arity::Unary, _) => write!(f, "a1"),
            Adverb(Arity::Binary, _) => write!(f, "a2"),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    DidNotFullyReduce(Vec<(Term, PartOfSpeech)>),
    ArrayLiteralNotNoun,
}

fn parse_word(word: Word) -> Result<(Term, PartOfSpeech), ParseError> {
    use PartOfSpeech::*;

    match word {
        Word::Int64(num) => Ok((Term::Atom(Atom::Int64(num)), Noun)),
        Word::Identifier(id) => {
            let pos = match id.as_str() {
                "+" | "*" => Verb(Arity::Binary),
                "neg" | "sign" => Verb(Arity::Unary),
                "." => Adverb(Arity::Binary, Arity::Binary),
                "fold" => Adverb(Arity::Unary, Arity::Unary),
                "flip" => Adverb(Arity::Unary, Arity::Binary),
                _ => Noun,
            };
            Ok((Term::Atom(Atom::Identifier(id)), pos))
        }
        Word::Parens(mut words) => {
            if words.is_empty() {
                Ok((Term::Parens(Box::new(Term::Tuple(vec![]))), Noun))
            } else {
                let (term, pos) = parse_words(&mut words)?;
                Ok((Term::Parens(Box::new(term)), pos))
            }
        }
        Word::Brackets(mut words) => {
            if words.is_empty() {
                Ok((Term::Brackets(vec![]), Noun))
            } else {
                let (term, pos) = parse_words(&mut words)?;
                if pos != Noun {
                    Err(ParseError::ArrayLiteralNotNoun)
                } else {
                    let terms = match term {
                        Term::Tuple(terms) => terms,
                        term => vec![term],
                    };
                    Ok((Term::Brackets(terms), pos))
                }
            }
        }
    }
}

fn parse_words(input: &mut Vec<Word>) -> Result<(Term, PartOfSpeech), ParseError> {
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
        //
        // It might be worth, you know, fixing this at some point.
        match &stack[stack.len() - 4..] {
            [.., Some((verb, Verb(_))), Some((adverb, Adverb(Unary, result_arity)))] => {
                let result_arity = *result_arity;
                // TODO: we can pretty easily avoid cloning the terms. slash,
                // like, this entire situation can be greatly simplified by a
                // typed stack helper thing
                let adverb = adverb.clone();
                let verb = verb.clone();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    UnaryApplication(Box::new(adverb), Box::new(verb)),
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
                    UnaryApplication(Box::new(verb), Box::new(noun)),
                    Noun,
                )));
                stack.push(stash);
            }
            [.., Some((rhs, Noun | Verb(_))), Some((verb, Adverb(Binary, result_arity))), Some((lhs, Noun | Verb(_))), _] =>
            {
                let lhs = lhs.clone();
                let verb = verb.clone();
                let result_arity = *result_arity;
                let rhs = rhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    BinaryApplication(Box::new(verb), Box::new(lhs), Box::new(rhs)),
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
                    BinaryApplication(Box::new(verb), Box::new(lhs), Box::new(rhs)),
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

                let result = match second {
                    Tuple(mut terms) => {
                        terms.push(first);
                        Tuple(terms)
                    }
                    _ => Tuple(vec![second, first]),
                };

                stack.push(Some((result, Noun)));
                stack.push(stash);
            }

            [.., Some((g, Verb(Unary))), Some((f, Verb(Unary))), None] => {
                let f = f.clone();
                let g = g.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    BinaryApplication(
                        Box::new(Implicit(Builtin::Compose)),
                        Box::new(f),
                        Box::new(g),
                    ),
                    Verb(Unary),
                )));
                stack.push(stash);
            }

            [.., Some((rhs, Noun)), Some((verb, Verb(Binary))), None] => {
                let verb = verb.clone();
                let rhs = rhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    BinaryApplication(
                        Box::new(Implicit(Builtin::PartialApplicationRight)),
                        Box::new(verb),
                        Box::new(rhs),
                    ),
                    Verb(Unary),
                )));
                stack.push(stash);
            }

            [.., Some((verb, Verb(Binary))), Some((lhs, Noun)), None] => {
                let verb = verb.clone();
                let lhs = lhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    BinaryApplication(
                        Box::new(Implicit(Builtin::PartialApplicationLeft)),
                        Box::new(verb),
                        Box::new(lhs),
                    ),
                    Verb(Unary),
                )));
                stack.push(stash);
            }

            [.., Some((rhs, Verb(Unary))), Some((verb, Verb(Binary))), None] => {
                let verb = verb.clone();
                let rhs = rhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    BinaryApplication(
                        Box::new(Implicit(Builtin::ComposeRight)),
                        Box::new(verb),
                        Box::new(rhs),
                    ),
                    Verb(Binary),
                )));
                stack.push(stash);
            }

            [.., Some((verb, Verb(Binary))), Some((lhs, Verb(Unary))), None] => {
                let verb = verb.clone();
                let lhs = lhs.clone();
                let stash = stack.pop().unwrap();
                stack.pop().unwrap();
                stack.pop().unwrap();
                stack.push(Some((
                    BinaryApplication(
                        Box::new(Implicit(Builtin::ComposeLeft)),
                        Box::new(verb),
                        Box::new(lhs),
                    ),
                    Verb(Binary),
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
                Some(word) => {
                    stack.push(Some(parse_word(word)?));
                }
            },
        };
    }
    let without_sentinels = stack.into_iter().flatten().collect::<Vec<_>>();
    match without_sentinels.len() {
        0 => panic!("empty parse"),
        1 => Ok(without_sentinels.into_iter().next().unwrap()),
        _ => Err(ParseError::DidNotFullyReduce(without_sentinels)),
    }
}

pub fn parse_tokens(tokens: Vec<Token>) -> Result<(Term, PartOfSpeech), ParseError> {
    let mut words = resolve_semicolons(tokens, Delimiter::Parens);
    parse_words(&mut words)
}

#[cfg(test)]
mod tests {
    use super::super::tokenizer;
    use super::*;

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

    fn delimited(start: &str, words: &[Word], end: &str) -> String {
        let mut result = start.to_string();
        result += &words
            .iter()
            .map(word_short_string)
            .collect::<Vec<_>>()
            .join(" ");
        result += end;
        result
    }

    fn word_short_string(word: &Word) -> String {
        match word {
            Word::Int64(num) => num.to_string(),
            Word::Identifier(id) => id.to_string(),
            Word::Parens(words) => delimited("(", words, ")"),
            Word::Brackets(words) => delimited("[", words, "]"),
        }
    }

    fn test(input: &str) -> String {
        let (remaining, tokens) = tokenizer::tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("unable to tokenize");
        }
        delimited("", &resolve_semicolons(tokens, Delimiter::Parens), "")
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
        match parse_words(&mut words) {
            Ok(term) => show_annotated_term(&term),
            Err(ParseError::DidNotFullyReduce(terms)) => {
                format!("incomplete parse: {}", show_annotated_terms(terms))
            }
            Err(error) => format!("error: {:?}", error),
        }
    }

    #[test]
    fn test_parser() {
        k9::snapshot!(tester("neg 1 + 2"), "n:(neg (+ 1 2))");
        k9::snapshot!(tester("fold +"), "v1:(fold +)");
        k9::snapshot!(tester("fold + x"), "n:((fold +) x)");
        k9::snapshot!(tester("x + y"), "n:(+ x y)");
        k9::snapshot!(tester("x +.* y"), "n:((. + *) x y)");
        k9::snapshot!(tester("x fold + . * y"), "n:((. (fold +) *) x y)");
        k9::snapshot!(tester("x + . fold * y"), "n:((. + (fold *)) x y)");
        k9::snapshot!(
            tester("x fold + . fold * y"),
            "n:((. (fold +) (fold *)) x y)"
        );
        k9::snapshot!(
            tester("x fold * . fold + . fold * y"),
            "n:((. (fold *) (. (fold +) (fold *))) x y)"
        );
    }

    #[test]
    fn test_adverbs() {
        k9::snapshot!(tester("1 + 2"), "n:(+ 1 2)");
        k9::snapshot!(tester("1 flip + 2"), "n:((flip +) 1 2)");
    }

    #[test]
    fn test_tuples() {
        k9::snapshot!(tester("1 + 1 2"), "n:(+ 1 (<tuple> 1 2))");
        k9::snapshot!(tester("1 + 1 2 3 4 5"), "n:(+ 1 (<tuple> 1 2 3 4 5))");
        k9::snapshot!(tester("1 + 1 neg 2"), "n:(+ 1 (<tuple> 1 (neg 2)))");
        k9::snapshot!(tester("1 2 + 1 2"), "n:(+ (<tuple> 1 2) (<tuple> 1 2))");
        k9::snapshot!(tester("1 + (1 2) 3"), "n:(+ 1 (<tuple> (<tuple> 1 2) 3))");
        k9::snapshot!(tester("1 + (1 2 3)"), "n:(+ 1 (<tuple> 1 2 3))");
        k9::snapshot!(tester("1 + 2; 3"), "n:(<tuple> (+ 1 2) 3)");
    }

    #[test]
    fn test_operator_sections() {
        k9::snapshot!(tester("+ 1"), "v1:(<rhs> + 1)");
        k9::snapshot!(tester("+ 1 2"), "v1:(<rhs> + (<tuple> 1 2))");
        k9::snapshot!(tester("(+ 1) 2"), "n:((<rhs> + 1) 2)");
        k9::snapshot!(tester("1 +"), "v1:(<lhs> + 1)");
        k9::snapshot!(tester("1 2 +"), "v1:(<lhs> + (<tuple> 1 2))");

        k9::snapshot!(tester("flip + 1"), "v1:(<rhs> (flip +) 1)");
        k9::snapshot!(tester("flip + 1 2"), "v1:(<rhs> (flip +) (<tuple> 1 2))");
        k9::snapshot!(tester("(flip + 1) 2"), "n:((<rhs> (flip +) 1) 2)");
        k9::snapshot!(tester("1 flip +"), "v1:(<lhs> (flip +) 1)");
        k9::snapshot!(tester("1 2 flip +"), "v1:(<lhs> (flip +) (<tuple> 1 2))");
    }

    #[test]
    fn test_implicit_composition() {
        k9::snapshot!(tester("neg sign"), "v1:(<comp> neg sign)");
        k9::snapshot!(tester("neg sign neg"), "v1:(<comp> (<comp> neg sign) neg)");
        k9::snapshot!(
            tester("neg + sign"),
            "v2:(<comp-rhs> (<comp-lhs> + neg) sign)"
        );
        k9::snapshot!(
            tester("neg sign + neg"),
            "v2:(<comp-rhs> (<comp-lhs> + (<comp> neg sign)) neg)"
        );
        k9::snapshot!(
            tester("neg + sign neg"),
            "v2:(<comp-rhs> (<comp-rhs> (<comp-lhs> + neg) sign) neg)"
        );

        k9::snapshot!(
            tester("neg + (sign neg)"),
            "v2:(<comp-rhs> (<comp-lhs> + neg) (<comp> sign neg))"
        );

        k9::snapshot!(tester("+ neg"), "v2:(<comp-rhs> + neg)");
        k9::snapshot!(tester("neg +"), "v2:(<comp-lhs> + neg)");
        k9::snapshot!(tester("neg + 1"), "v1:(<rhs> (<comp-lhs> + neg) 1)");

        k9::snapshot!(tester("flip + neg"), "v2:(<comp-rhs> (flip +) neg)");
        k9::snapshot!(tester("neg flip +"), "v2:(<comp-lhs> (flip +) neg)");
        k9::snapshot!(
            tester("neg flip + 1"),
            "v1:(<rhs> (<comp-lhs> (flip +) neg) 1)"
        );
    }

    #[test]
    fn test_array_literals() {
        k9::snapshot!(tester("[]"), "n:[]");
        k9::snapshot!(tester("[1 2 3]"), "n:[1 2 3]");
        k9::snapshot!(tester("[1 2 3; 4 5 6]"), "n:[[1 2 3] [4 5 6]]");
        k9::snapshot!(
            tester("[1 2 3; 4 5 6;; 7 8 9; 10 11 12]"),
            "n:[[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]]"
        );
    }

    #[test]
    fn test_confusing_expressions() {
        k9::snapshot!(tester("* 1 +"), "v2:(<comp-lhs> + (<rhs> * 1))");
        // TODO: this should parse. if the other thing parses, this should parse.
        k9::snapshot!(tester("* + 1"), "incomplete parse: n:1 v2:+ v2:*");
        k9::snapshot!(tester("1 * +"), "v2:(<comp-lhs> + (<lhs> * 1))");
    }

    #[test]
    fn test_parse_errors() {
        k9::snapshot!(tester("* +"), "incomplete parse: v2:+ v2:*");
        k9::snapshot!(tester("* flip +"), "incomplete parse: v2:(flip +) v2:*");
        k9::snapshot!(tester(". +"), "incomplete parse: v2:+ a2:.");
        k9::snapshot!(tester("+ ."), "incomplete parse: a2:. v2:+");
        k9::snapshot!(tester("flip ."), "incomplete parse: a2:. a1:flip");
        k9::snapshot!(tester("fold ."), "incomplete parse: a2:. a1:fold");
        k9::snapshot!(tester(". flip"), "incomplete parse: a1:flip a2:.");
        k9::snapshot!(tester(". fold"), "incomplete parse: a1:fold a2:.");
        k9::snapshot!(tester("flip fold"), "incomplete parse: a1:fold a1:flip");
    }
}
