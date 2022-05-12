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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Delimiter {
    Parens,
    Brackets,
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
use PartOfSpeech::*;

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

#[derive(Debug)]
pub enum ParseError {
    DidNotFullyReduce(Vec<(Term, PartOfSpeech)>),
    ArrayLiteralNotNoun,
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Int64(num) => write!(f, "{}", num),
            Atom::Identifier(id) => write!(f, "{}", id),
        }
    }
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

impl fmt::Display for PartOfSpeech {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Noun => write!(f, "n"),
            Verb(Arity::Unary) => write!(f, "v1"),
            Verb(Arity::Binary) => write!(f, "v2"),
            Adverb(Arity::Unary, _) => write!(f, "a1"),
            Adverb(Arity::Binary, _) => write!(f, "a2"),
        }
    }
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

struct ParseFrame {
    stack: Vec<Option<(Term, PartOfSpeech)>>,
    input: Vec<Word>,
    end_reached: bool,
    finish: fn(Term, PartOfSpeech) -> Result<Term, ParseError>,
}

impl ParseFrame {
    fn new(input: Vec<Word>, finish: fn(Term, PartOfSpeech) -> Result<Term, ParseError>) -> Self {
        Self {
            input,
            end_reached: false,
            stack: vec![None, None, None, None],
            finish,
        }
    }
}

enum ParseResult {
    Complete(Term, PartOfSpeech),
    Partial(String, Vec<ParseFrame>),
}

fn identity(term: Term, _: PartOfSpeech) -> Result<Term, ParseError> {
    Ok(term)
}

fn wrap_parens(term: Term, _: PartOfSpeech) -> Result<Term, ParseError> {
    Ok(Term::Parens(Box::new(term)))
}

fn wrap_brackets(term: Term, pos: PartOfSpeech) -> Result<Term, ParseError> {
    match pos {
        Noun => {
            let terms = match term {
                Term::Tuple(terms) => terms,
                term => vec![term],
            };
            Ok(Term::Brackets(terms))
        }
        _ => Err(ParseError::ArrayLiteralNotNoun),
    }
}

fn pop_term(stack: &mut Vec<Option<(Term, PartOfSpeech)>>) -> Term {
    let (term, _) = stack.pop().unwrap().unwrap();
    term
}

fn pop_adverb(stack: &mut Vec<Option<(Term, PartOfSpeech)>>) -> (Term, Arity) {
    let (term, pos) = stack.pop().unwrap().unwrap();
    match pos {
        Adverb(_, result_arity) => (term, result_arity),
        _ => panic!("not an adverb!"),
    }
}

macro_rules! lookahead {
    ($stack:ident, $block:block) => {{
        let stash = $stack.pop().unwrap();
        $block;
        $stack.push(stash);
    }};
}

macro_rules! bin_impl_lr {
    ($stack:ident, $inner:path, $pos:expr) => {
        let lhs = pop_term($stack);
        let rhs = pop_term($stack);
        $stack.push(Some((
            BinaryApplication(
                Box::new(Term::Implicit($inner)),
                Box::new(lhs),
                Box::new(rhs),
            ),
            $pos,
        )));
    };
}

macro_rules! bin_impl_rl {
    ($stack:ident, $inner:expr, $pos:expr) => {
        let rhs = pop_term($stack);
        let lhs = pop_term($stack);
        $stack.push(Some((
            BinaryApplication(
                Box::new(Term::Implicit($inner)),
                Box::new(lhs),
                Box::new(rhs),
            ),
            $pos,
        )));
    };
}

macro_rules! pos {
    (s) => {
        None
    };

    (sn) => {
        pos!(s) | pos!(n)
    };

    (sv) => {
        pos!(s) | pos!(v)
    };

    (svn) => {
        pos!(s) | pos!(v) | pos!(n)
    };

    (vn) => {
        pos!(v) | pos!(n)
    };

    (n) => {
        Some((_, Noun))
    };

    (v) => {
        Some((_, Verb(_)))
    };

    (v1) => {
        Some((_, Verb(Unary)))
    };

    (v2) => {
        Some((_, Verb(Binary)))
    };

    (a1) => {
        Some((_, Adverb(Unary, _)))
    };

    (a2) => {
        Some((_, Adverb(Binary, _)))
    };

    (_) => {
        _
    };
}

macro_rules! stack {
    (@private [ $($rev:pat),* ], $x:tt $(,$xs:tt)*) => {
        stack!(@private [ pos!($x) $(,$rev)* ] $(,$xs)*)
    };

    (@private [ $($rev:pat),* ]) => {
        [.., $($rev),*]
    };

    ($($xs:tt),+) => {
        stack!(@private [], $($xs),*)
    };
}

fn reduce_stack(stack: &mut Vec<Option<(Term, PartOfSpeech)>>) {
    use Arity::*;
    use Term::*;

    loop {
        match &stack[stack.len() - 4..] {
            stack![a1, v] => {
                let (adverb, result_arity) = pop_adverb(stack);
                let verb = pop_term(stack);
                stack.push(Some((
                    UnaryApplication(Box::new(adverb), Box::new(verb)),
                    Verb(result_arity),
                )));
            }

            stack![svn, v1, n] => lookahead!(stack, {
                let verb = pop_term(stack);
                let noun = pop_term(stack);
                stack.push(Some((
                    UnaryApplication(Box::new(verb), Box::new(noun)),
                    Noun,
                )));
            }),

            stack![_, vn, a2, vn] => lookahead!(stack, {
                let lhs = pop_term(stack);
                let (conjunction, result_arity) = pop_adverb(stack);
                let rhs = pop_term(stack);
                stack.push(Some((
                    BinaryApplication(Box::new(conjunction), Box::new(lhs), Box::new(rhs)),
                    Verb(result_arity),
                )));
            }),

            stack![sv, n, v2, n] => lookahead!(stack, {
                let lhs = pop_term(stack);
                let verb = pop_term(stack);
                let rhs = pop_term(stack);
                stack.push(Some((
                    BinaryApplication(Box::new(verb), Box::new(lhs), Box::new(rhs)),
                    Noun,
                )));
            }),

            stack![svn, n, n] => lookahead!(stack, {
                let first = pop_term(stack);
                let second = pop_term(stack);

                let result = match second {
                    Tuple(mut terms) => {
                        terms.push(first);
                        Tuple(terms)
                    }
                    _ => Tuple(vec![second, first]),
                };

                stack.push(Some((result, Noun)));
            }),

            stack![sv, v1, v1] => lookahead!(stack, {
                bin_impl_lr!(stack, Builtin::Compose, Verb(Unary));
            }),

            stack![sv, v2, n] => lookahead!(stack, {
                bin_impl_lr!(stack, Builtin::PartialApplicationRight, Verb(Unary));
            }),

            stack![sv, n, v2] => lookahead!(stack, {
                bin_impl_rl!(stack, Builtin::PartialApplicationLeft, Verb(Unary));
            }),

            stack![sv, v2, v1] => lookahead!(stack, {
                bin_impl_lr!(stack, Builtin::ComposeRight, Verb(Binary));
            }),

            stack![sv, v1, v2] => lookahead!(stack, {
                bin_impl_rl!(stack, Builtin::ComposeLeft, Verb(Binary));
            }),

            _ => break,
        }
    }
}

fn parse(mut call_stack: Vec<ParseFrame>) -> Result<ParseResult, ParseError> {
    loop {
        let state = call_stack.last_mut().unwrap();

        reduce_stack(&mut state.stack);

        match state.input.pop() {
            None => {
                if state.end_reached {
                    let state = call_stack.pop().unwrap();
                    let without_sentinels = state.stack.into_iter().flatten().collect::<Vec<_>>();
                    let (term, pos) = match without_sentinels.len() {
                        0 => Ok((Term::Tuple(vec![]), Noun)),
                        1 => Ok(without_sentinels.into_iter().next().unwrap()),
                        _ => Err(ParseError::DidNotFullyReduce(without_sentinels)),
                    }?;
                    let term = (state.finish)(term, pos)?;

                    match call_stack.last_mut() {
                        None => return Ok(ParseResult::Complete(term, pos)),
                        Some(next) => next.stack.push(Some((term, pos))),
                    }
                } else {
                    state.end_reached = true;
                    state.stack.push(None);
                }
            }

            Some(word) => match word {
                Word::Int64(num) => state.stack.push(Some((Term::Atom(Atom::Int64(num)), Noun))),
                Word::Identifier(id) => return Ok(ParseResult::Partial(id, call_stack)),
                Word::Parens(words) => call_stack.push(ParseFrame::new(words, wrap_parens)),
                Word::Brackets(words) => call_stack.push(ParseFrame::new(words, wrap_brackets)),
            },
        };
    }
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

    fn parse_to_completion(input: Vec<Word>) -> Result<(Term, PartOfSpeech), ParseError> {
        use ParseResult::*;

        let state = ParseFrame::new(input, identity);
        let mut call_stack = vec![state];

        loop {
            match parse(call_stack)? {
                Complete(term, pos) => return Ok((term, pos)),
                Partial(id, stack) => {
                    call_stack = stack;
                    let pos = match id.as_str() {
                        "+" | "*" => Verb(Arity::Binary),
                        "neg" | "sign" => Verb(Arity::Unary),
                        "." => Adverb(Arity::Binary, Arity::Binary),
                        "fold" => Adverb(Arity::Unary, Arity::Unary),
                        "flip" => Adverb(Arity::Unary, Arity::Binary),
                        "x" | "y" => Noun,
                        _ => panic!("unknown identifier"),
                    };
                    let term = Term::Atom(Atom::Identifier(id));
                    let top_state = call_stack.last_mut().unwrap();
                    top_state.stack.push(Some((term, pos)));
                }
            }
        }
    }

    fn tester(input: &str) -> String {
        let (remaining, tokens) = tokenizer::tokens(input).unwrap();
        if !remaining.is_empty() {
            panic!("not a total parse!");
        }
        let words = resolve_semicolons(tokens, Delimiter::Parens);
        match parse_to_completion(words) {
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
    fn test_unary_composition() {
        k9::snapshot!(tester("neg sign"), "v1:(<comp> neg sign)");
        k9::snapshot!(tester("neg sign neg"), "v1:(<comp> neg (<comp> sign neg))");

        k9::snapshot!(tester("fold + fold *"), "v1:(<comp> (fold +) (fold *))");
        k9::snapshot!(
            tester("fold + fold * fold +"),
            "v1:(<comp> (fold +) (<comp> (fold *) (fold +)))"
        );
    }

    #[test]
    fn test_implicit_composition() {
        k9::snapshot!(
            tester("neg + sign"),
            "v2:(<comp-lhs> (<comp-rhs> + sign) neg)"
        );
        k9::snapshot!(
            tester("neg sign + neg"),
            "v2:(<comp-lhs> (<comp-lhs> (<comp-rhs> + neg) sign) neg)"
        );
        k9::snapshot!(
            tester("neg + sign neg"),
            "v2:(<comp-lhs> (<comp-rhs> + (<comp> sign neg)) neg)"
        );

        k9::snapshot!(
            tester("neg + (sign neg)"),
            "v2:(<comp-lhs> (<comp-rhs> + (<comp> sign neg)) neg)"
        );

        k9::snapshot!(tester("+ neg"), "v2:(<comp-rhs> + neg)");
        k9::snapshot!(tester("neg +"), "v2:(<comp-lhs> + neg)");
        k9::snapshot!(tester("neg + 1"), "v1:(<comp> neg (<rhs> + 1))");

        k9::snapshot!(tester("flip + neg"), "v2:(<comp-rhs> (flip +) neg)");
        k9::snapshot!(tester("neg flip +"), "v2:(<comp-lhs> (flip +) neg)");
        k9::snapshot!(tester("neg flip + 1"), "v1:(<comp> neg (<rhs> (flip +) 1))");
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
        k9::snapshot!(tester("* 1 +"), "v2:(<comp-rhs> * (<lhs> + 1))");
        k9::snapshot!(tester("* + 1"), "v2:(<comp-rhs> * (<rhs> + 1))");
        k9::snapshot!(tester("1 * +"), "v2:(<comp-lhs> + (<lhs> * 1))");
    }

    #[test]
    fn test_implicit_equivalences() {
        k9::snapshot!(
            tester("neg + sign"),
            "v2:(<comp-lhs> (<comp-rhs> + sign) neg)"
        );
        k9::snapshot!(
            tester("neg (+ sign)"),
            "v2:(<comp-lhs> (<comp-rhs> + sign) neg)"
        );

        k9::snapshot!(tester("neg + 1"), "v1:(<comp> neg (<rhs> + 1))");
        k9::snapshot!(tester("neg (+ 1)"), "v1:(<comp> neg (<rhs> + 1))");

        k9::snapshot!(tester("+ sign neg"), "v2:(<comp-rhs> + (<comp> sign neg))");
        k9::snapshot!(
            tester("+ (sign neg)"),
            "v2:(<comp-rhs> + (<comp> sign neg))"
        );
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
