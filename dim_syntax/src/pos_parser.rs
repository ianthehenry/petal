use super::old_tokenizer::OldToken;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};

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
    Identifier(RichIdentifier),
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
    BadReference(Identifier),
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

// TODO: is there a good reason to do this ahead of time in a separate pass? why
// not just do it in the middle of parsing?
fn resolve_semicolons(tokens: Vec<OldToken>, delimiter: Delimiter) -> Vec<Word> {
    let mut index_levels: Vec<usize> = vec![];
    let mut words: Vec<Word> = vec![];
    for token in tokens {
        match token {
            OldToken::Int64(value) => words.push(Word::Int64(value)),
            OldToken::Parens(tokens) => {
                words.push(Word::Parens(resolve_semicolons(tokens, Delimiter::Parens)))
            }
            OldToken::Brackets(tokens) => words.push(Word::Brackets(resolve_semicolons(
                tokens,
                Delimiter::Brackets,
            ))),
            OldToken::Identifier(value) => words.push(Word::Identifier(value)),
            OldToken::Semicolons(level) => {
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

#[derive(Debug)]
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

pub fn just_parse(tokens: Vec<OldToken>) -> Result<(Term, PartOfSpeech), ParseError> {
    let words = resolve_semicolons(tokens, Delimiter::Parens);
    let frame = ParseFrame::new(words, identity);
    match parse(vec![frame])? {
        ParseResult::Complete(term, pos) => Ok((term, pos)),
        ParseResult::Partial(_, _) => panic!("partial parse"),
    }
}

fn parse(mut call_stack: Vec<ParseFrame>) -> Result<ParseResult, ParseError> {
    loop {
        let frame = call_stack.last_mut().unwrap();

        reduce_stack(&mut frame.stack);

        match frame.input.pop() {
            None => {
                if frame.end_reached {
                    let frame = call_stack.pop().unwrap();
                    let without_sentinels = frame.stack.into_iter().flatten().collect::<Vec<_>>();
                    let (term, pos) = match without_sentinels.len() {
                        0 => Ok((Term::Tuple(vec![]), Noun)),
                        1 => Ok(without_sentinels.into_iter().next().unwrap()),
                        _ => Err(ParseError::DidNotFullyReduce(without_sentinels)),
                    }?;
                    let term = (frame.finish)(term, pos)?;

                    match call_stack.last_mut() {
                        None => return Ok(ParseResult::Complete(term, pos)),
                        Some(next) => next.stack.push(Some((term, pos))),
                    }
                } else {
                    frame.end_reached = true;
                    frame.stack.push(None);
                }
            }

            Some(word) => match word {
                Word::Int64(num) => frame.stack.push(Some((Term::Atom(Atom::Int64(num)), Noun))),
                Word::Identifier(id) => return Ok(ParseResult::Partial(id, call_stack)),
                Word::Parens(words) => call_stack.push(ParseFrame::new(words, wrap_parens)),
                Word::Brackets(words) => call_stack.push(ParseFrame::new(words, wrap_brackets)),
            },
        };
    }
}

struct Assignment {
    name: String,
    expression: Vec<Word>,
}

// "partial" means that it's waiting for an identifier that has not been seen at
// all yet. "semipartial" means that we know *which* identifier we're waiting
// for, but we don't know its part of speech yet.
enum ParseStatus {
    Complete(Term, PartOfSpeech),
    Partial(String, Vec<ParseFrame>),
    Semipartial(Identifier, Vec<ParseFrame>),
    Failed(ParseError),
}

type Identifier = u64;

#[derive(Eq, Debug, Clone)]
pub struct RichIdentifier {
    id: Identifier,
    name: String,
}

impl RichIdentifier {
    fn new(id: Identifier, name: String) -> Self {
        RichIdentifier { id, name }
    }
}

impl fmt::Display for RichIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for RichIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for RichIdentifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

struct ParseOperation {
    id: Identifier,
    call_stack: Vec<ParseFrame>,
}

impl ParseOperation {
    fn new(id: Identifier, call_stack: Vec<ParseFrame>) -> Self {
        ParseOperation { id, call_stack }
    }
}

struct Scope {
    name_to_ids: HashMap<String, Vec<Identifier>>,
    id_to_name: HashMap<Identifier, String>,

    blocked_on_name: HashMap<String, Vec<ParseOperation>>,
    blocked_on_id: HashMap<Identifier, Vec<ParseOperation>>,
    complete: HashMap<Identifier, (Term, PartOfSpeech)>,
    failed: HashMap<Identifier, ParseError>,
    unblocked: Vec<ParseOperation>,

    parent_scope: Option<Rc<Scope>>,
    allocator: Rc<RefCell<Allocator>>,
}

struct Allocator {
    current: Identifier,
}

impl Allocator {
    fn new() -> Self {
        Allocator { current: 0 }
    }
    fn next(&mut self) -> Identifier {
        let x = self.current;
        self.current += 1;
        x
    }
}

enum LookupResult<'a> {
    Unknown,
    Pending(Identifier),
    Failed(Identifier, &'a ParseError),
    Complete(Identifier, &'a Term, PartOfSpeech),
}

impl Scope {
    fn new(parent_scope: Option<Rc<Scope>>) -> Scope {
        Scope {
            name_to_ids: HashMap::new(),
            id_to_name: HashMap::new(),
            allocator: match &parent_scope {
                None => Rc::new(RefCell::new(Allocator::new())),
                Some(parent_scope) => parent_scope.allocator.clone(),
            },
            parent_scope,
            blocked_on_name: HashMap::new(),
            blocked_on_id: HashMap::new(),
            complete: HashMap::new(),
            failed: HashMap::new(),
            unblocked: vec![],
        }
    }

    fn add_builtin(&mut self, name: &str, pos: PartOfSpeech) {
        let name = name.to_string();
        let id = self.learn_name(name.clone());
        self.complete.insert(
            id,
            (
                Term::Atom(Atom::Identifier(RichIdentifier::new(id, name))),
                pos,
            ),
        );
    }

    fn begin(&mut self, assignment: Assignment) {
        let Assignment { name, expression } = assignment;
        let frame = ParseFrame::new(expression, identity);
        let call_stack = vec![frame];
        let id = self.learn_name(name);
        self.unblocked.push(ParseOperation { id, call_stack });
    }

    fn blocked_on_name(&mut self, prereq_name: String, parse: ParseOperation) {
        self.blocked_on_name
            .entry(prereq_name)
            .or_insert_with(Vec::new)
            .push(parse);
    }

    fn blocked_on_id(&mut self, prereq_id: Identifier, parse: ParseOperation) {
        self.blocked_on_id
            .entry(prereq_id)
            .or_insert_with(Vec::new)
            .push(parse);
    }

    fn failed(&mut self, id: Identifier, error: ParseError) {
        if let Some(parses) = self.blocked_on_id.remove(&id) {
            for parse in parses {
                self.failed.insert(parse.id, ParseError::BadReference(id));
            }
        }

        assert!(self.failed.insert(id, error).is_none());
    }

    fn name_of_id(&self, id: &Identifier) -> String {
        if let Some(name) = self.id_to_name.get(id) {
            return name.clone();
        }
        match &self.parent_scope {
            None => panic!("identifier not found"),
            Some(scope) => scope.name_of_id(id),
        }
    }

    fn complete(&mut self, id: Identifier, term: Term, pos: PartOfSpeech) {
        let rich_id = RichIdentifier {
            name: self.name_of_id(&id),
            id,
        };
        if let Some(parses) = self.blocked_on_id.remove(&id) {
            for mut parse in parses {
                provide(
                    &mut parse.call_stack,
                    Term::Atom(Atom::Identifier(rich_id.clone())),
                    pos,
                );
                self.unblocked.push(parse);
            }
        }
        assert!(self.complete.insert(id, (term, pos)).is_none());
    }

    fn lookup_previous_identifier(&self, name: &str, as_of: Identifier) -> Option<Identifier> {
        match self.name_to_ids.get(name) {
            Some(bindings) => bindings
                .iter()
                .filter(|id| **id < as_of)
                .map(Identifier::clone)
                .last(),
            None => match &self.parent_scope {
                None => None,
                Some(scope) => scope.lookup_previous_identifier(name, as_of),
            },
        }
    }

    fn lookup_next_identifier(&self, name: &str, as_of: Identifier) -> Option<Identifier> {
        match self.name_to_ids.get(name) {
            Some(bindings) => bindings
                .iter()
                .filter(|id| **id >= as_of)
                .map(Identifier::clone)
                .next(),
            None => match &self.parent_scope {
                None => None,
                Some(scope) => scope.lookup_next_identifier(name, as_of),
            },
        }
    }

    // TODO: this is stupidly (number of definitions * depth of scope). because
    // everything is sorted, this could easily be (log(number of definitions) *
    // depth of scope)
    fn lookup_identifier(&self, name: &str, as_of: Identifier) -> Option<Identifier> {
        match self.lookup_previous_identifier(name, as_of) {
            Some(id) => Some(id),
            None => self.lookup_next_identifier(name, as_of),
        }
    }

    fn lookup_by_id(&self, id: Identifier) -> LookupResult {
        if let Some((term, pos)) = self.complete.get(&id) {
            return LookupResult::Complete(id, term, *pos);
        }
        if let Some(error) = self.failed.get(&id) {
            return LookupResult::Failed(id, error);
        }
        // a more "obvious" approach would be to check the two "blocked" keys
        // for the Pending result and then panic if we never find something. but
        // that would require either linearly scanning the blocked dictionaries
        // or storing an extra map. so we're taking advantage of the invariant
        // that we only have Identifiers for names that are pending
        match &self.parent_scope {
            None => LookupResult::Pending(id),
            Some(scope) => scope.lookup_by_id(id),
        }
    }

    fn lookup(&self, name: &str, as_of: Identifier) -> LookupResult {
        match self.lookup_identifier(name, as_of) {
            Some(id) => self.lookup_by_id(id),
            None => LookupResult::Unknown,
        }
    }

    fn learn_name(&mut self, name: String) -> Identifier {
        let id = self.allocator.borrow_mut().next();

        assert!(self.id_to_name.insert(id, name.clone()).is_none());

        if let Some(parses) = self.blocked_on_name.remove(&name) {
            for parse in parses {
                self.blocked_on_id(id, parse);
            }
        }
        let vec = self.name_to_ids.entry(name).or_insert_with(Vec::new);
        vec.push(id);
        id
    }
}

fn parse_body(mut scope: Scope, assignments: Vec<Assignment>) -> Scope {
    let mut input_queue = assignments;
    // we have to process top-to-bottom
    input_queue.reverse();

    while let Some(assignment) = input_queue.pop() {
        scope.begin(assignment);
        while let Some(ParseOperation { id, mut call_stack }) = scope.unblocked.pop() {
            loop {
                match parse(call_stack) {
                    Err(e) => {
                        scope.failed(id, e);
                        break;
                    }
                    Ok(ParseResult::Complete(term, pos)) => {
                        scope.complete(id, term, pos);
                        break;
                    }
                    Ok(ParseResult::Partial(prereq_name, stack)) => {
                        // TODO: should add support for "not yet parsed but part of
                        // speech already known"
                        match scope.lookup(&prereq_name, id) {
                            LookupResult::Unknown => {
                                scope.blocked_on_name(prereq_name, ParseOperation::new(id, stack));
                                break;
                            }
                            LookupResult::Pending(prereq_id) => {
                                scope.blocked_on_id(prereq_id, ParseOperation::new(id, stack));
                                break;
                            }
                            LookupResult::Failed(prereq_id, _) => {
                                scope.failed(id, ParseError::BadReference(prereq_id));
                                break;
                            }
                            LookupResult::Complete(prereq_id, _term, pos) => {
                                call_stack = stack;
                                provide(
                                    &mut call_stack,
                                    Term::Atom(Atom::Identifier(RichIdentifier::new(
                                        prereq_id,
                                        prereq_name,
                                    ))),
                                    pos,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    // If any assignment failed, the whole parse failed.
    //
    // Otherwise, if something is blocked on name, we need to return pending.
    //
    // Otherwise, if something is blocked on an ID defined in a parent scope,
    // then we need to return pending.
    //
    // Otherwise, if something is blocked on an ID defined in *my* scope,
    // there's a cyclic problem and we can error immediately.
    //
    // Otherwise, we successfully parsed every assignment.

    scope
}

fn provide(call_stack: &mut Vec<ParseFrame>, term: Term, pos: PartOfSpeech) {
    let top_frame = call_stack.last_mut().unwrap();
    top_frame.stack.push(Some((term, pos)));
}

#[cfg(test)]
mod tests {
    use super::super::old_tokenizer;
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
        let tokens = old_tokenizer::tokenize(input);
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

        let frame = ParseFrame::new(input, identity);
        let mut call_stack = vec![frame];

        loop {
            match parse(call_stack)? {
                Complete(term, pos) => return Ok((term, pos)),
                Partial(name, stack) => {
                    call_stack = stack;
                    let pos = match name.as_str() {
                        "+" | "*" => Verb(Arity::Binary),
                        "neg" | "sign" => Verb(Arity::Unary),
                        "." => Adverb(Arity::Binary, Arity::Binary),
                        "fold" => Adverb(Arity::Unary, Arity::Unary),
                        "flip" => Adverb(Arity::Unary, Arity::Binary),
                        "x" | "y" => Noun,
                        _ => panic!("unknown identifier"),
                    };
                    provide(
                        &mut call_stack,
                        Term::Atom(Atom::Identifier(RichIdentifier { name, id: 0 })),
                        pos,
                    );
                }
            }
        }
    }

    fn tester(input: &str) -> String {
        let tokens = old_tokenizer::tokenize(input);
        let words = resolve_semicolons(tokens, Delimiter::Parens);
        match parse_to_completion(words) {
            Ok(term) => show_annotated_term(&term),
            Err(ParseError::DidNotFullyReduce(terms)) => {
                format!("incomplete parse: {}", show_annotated_terms(terms))
            }
            Err(error) => format!("error: {:?}", error),
        }
    }

    fn begin_parse(input: &str) -> Vec<ParseFrame> {
        let tokens = old_tokenizer::tokenize(input);
        let words = resolve_semicolons(tokens, Delimiter::Parens);
        let frame = ParseFrame::new(words, identity);
        vec![frame]
    }

    fn advance(call_stack: Vec<ParseFrame>) -> (String, Option<Vec<ParseFrame>>) {
        match parse(call_stack) {
            Ok(ParseResult::Complete(term, pos)) => (show_annotated_term(&(term, pos)), None),
            Ok(ParseResult::Partial(id, stack)) => (format!("awaiting {}", id), Some(stack)),
            Err(ParseError::DidNotFullyReduce(terms)) => (
                format!("incomplete parse: {}", show_annotated_terms(terms)),
                None,
            ),
            Err(error) => (format!("error: {:?}", error), None),
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

    fn id(name: &str) -> Term {
        Term::Atom(Atom::Identifier(RichIdentifier {
            name: name.to_string(),
            id: 0,
        }))
    }

    #[test]
    fn test_partial_parsing() {
        let call_stack = begin_parse("x + foo");
        let (result, mut call_stack) = advance(call_stack);
        k9::snapshot!(result, "awaiting foo");
        provide(call_stack.as_mut().unwrap(), id("foo"), Noun);
        let (result, mut call_stack) = advance(call_stack.unwrap());
        k9::snapshot!(result, "awaiting +");
        provide(call_stack.as_mut().unwrap(), id("+"), Verb(Arity::Binary));
        let (result, mut call_stack) = advance(call_stack.unwrap());
        k9::snapshot!(result, "awaiting x");
        provide(call_stack.as_mut().unwrap(), id("x"), Noun);
        let (result, call_stack) = advance(call_stack.unwrap());
        k9::snapshot!(result, "n:(+ x foo)");
        k9::snapshot!(call_stack, "None");
    }

    fn assign(name: &str, expr: &str) -> Assignment {
        let tokens = old_tokenizer::tokenize(expr);
        let words = resolve_semicolons(tokens, Delimiter::Parens);
        Assignment {
            name: name.to_string(),
            expression: words,
        }
    }

    struct Disambiguator {
        name_indices: HashMap<String, u64>,
        name_seen_at: HashMap<RichIdentifier, u64>,
    }

    impl Disambiguator {
        fn new() -> Self {
            Disambiguator {
                name_indices: HashMap::new(),
                name_seen_at: HashMap::new(),
            }
        }

        fn see(&mut self, rich_id: RichIdentifier) {
            match self.name_seen_at.get(&rich_id) {
                None => {
                    let ix = self.name_indices.entry(rich_id.name.clone()).or_insert(0);
                    self.name_seen_at.insert(rich_id, *ix);
                    *ix = *ix + 1;
                }
                Some(name_index) => (),
            };
        }

        fn view(&self, rich_id: &RichIdentifier) -> String {
            let name_index = *self.name_seen_at.get(rich_id).unwrap();
            if name_index == 0 {
                rich_id.name.clone()
            } else {
                format!("{}_{}", rich_id.name, name_index)
            }
        }
    }

    #[derive(Debug)]
    enum AssignmentStatus<'a> {
        Complete(&'a Term, &'a PartOfSpeech),
        Failed(&'a ParseError),
        Cyclic(&'a Identifier),
        Pending(&'a str),
    }

    fn rewrite_atoms<F: FnMut(&Atom) -> Atom>(term: &Term, f: &mut F) -> Term {
        use Term::*;
        match term {
            Atom(a) => Atom(f(a)),
            Parens(term) => Parens(Box::new(rewrite_atoms(&*term, f))),
            Implicit(x) => Implicit(*x),
            Tuple(terms) => Tuple(terms.iter().map(|term| rewrite_atoms(term, f)).collect()),
            Brackets(terms) => Brackets(terms.iter().map(|term| rewrite_atoms(term, f)).collect()),
            UnaryApplication(term1, term2) => UnaryApplication(
                Box::new(rewrite_atoms(&*term1, f)),
                Box::new(rewrite_atoms(&*term2, f)),
            ),
            BinaryApplication(term1, term2, term3) => BinaryApplication(
                Box::new(rewrite_atoms(&*term1, f)),
                Box::new(rewrite_atoms(&*term2, f)),
                Box::new(rewrite_atoms(&*term3, f)),
            ),
        }
    }

    fn print_assignments(scope: &Scope) -> String {
        let mut disambiguator = Disambiguator::new();

        let completes = scope
            .complete
            .iter()
            .map(|(id, (term, pos))| (*id, AssignmentStatus::Complete(term, pos)));
        let failures = scope
            .failed
            .iter()
            .map(|(id, error)| (*id, AssignmentStatus::Failed(error)));
        let cyclics = scope.blocked_on_id.iter().flat_map(|(missing_id, parses)| {
            parses
                .iter()
                .map(|parse| (parse.id, AssignmentStatus::Cyclic(missing_id)))
        });
        let pendings = scope
            .blocked_on_name
            .iter()
            .flat_map(|(missing_name, parses)| {
                parses
                    .iter()
                    .map(|parse| (parse.id, AssignmentStatus::Pending(missing_name)))
            });

        let mut kvps = completes
            .chain(failures)
            .chain(cyclics)
            .chain(pendings)
            .collect::<Vec<_>>();
        kvps.sort_by_key(|x| x.0);
        let mut first = true;
        let mut result = String::new();

        for (id, status) in kvps {
            let name = scope.name_of_id(&id);
            let rich_id = RichIdentifier { id, name };
            disambiguator.see(rich_id.clone());

            if first {
                first = false;
            } else {
                result.push('\n');
            }

            match status {
                AssignmentStatus::Complete(term, pos) => {
                    // TODO: do this with mutation?
                    let mut f = |atom: &Atom| match atom {
                        Atom::Identifier(rich_id) => {
                            disambiguator.see(rich_id.clone());
                            Atom::Identifier(RichIdentifier {
                                id: rich_id.id,
                                name: disambiguator.view(rich_id),
                            })
                        }
                        _ => atom.clone(),
                    };
                    let term = rewrite_atoms(&term, &mut f);

                    result.push_str(&format!(
                        "{} ({}) = {}",
                        disambiguator.view(&rich_id),
                        pos,
                        term
                    ));
                }
                AssignmentStatus::Failed(ParseError::BadReference(prereq_id)) => {
                    let prereq_name = scope.name_of_id(prereq_id);
                    let rich_prereq_id = RichIdentifier {
                        id: *prereq_id,
                        name: prereq_name,
                    };
                    result.push_str(&format!(
                        "{} depends on failed {}",
                        disambiguator.view(&rich_id),
                        disambiguator.view(&rich_prereq_id)
                    ));
                }
                AssignmentStatus::Failed(error) => {
                    result.push_str(&format!(
                        "{} failed: {:?}",
                        disambiguator.view(&rich_id),
                        error
                    ));
                }
                AssignmentStatus::Cyclic(prereq_id) => {
                    let prereq_name = scope.name_of_id(prereq_id);
                    let rich_prereq_id = RichIdentifier {
                        id: *prereq_id,
                        name: prereq_name,
                    };
                    disambiguator.see(rich_prereq_id.clone());
                    result.push_str(&format!(
                        "{} depends on {}",
                        disambiguator.view(&rich_id),
                        disambiguator.view(&rich_prereq_id)
                    ));
                }
                AssignmentStatus::Pending(prereq_name) => {
                    result.push_str(&format!(
                        "{} depends on unseen {}",
                        disambiguator.view(&rich_id),
                        prereq_name
                    ));
                }
            }
        }
        result
    }

    fn test_body(assignments: Vec<Assignment>) -> String {
        let mut top_level_scope = Scope::new(None);
        top_level_scope.add_builtin("+", Verb(Arity::Binary));
        top_level_scope.add_builtin("*", Verb(Arity::Binary));
        top_level_scope.add_builtin(".", Adverb(Arity::Binary, Arity::Binary));
        top_level_scope.add_builtin("fold", Adverb(Arity::Unary, Arity::Unary));
        top_level_scope.add_builtin("flip", Adverb(Arity::Unary, Arity::Binary));
        top_level_scope.add_builtin("x", Noun);
        top_level_scope.add_builtin("y", Noun);
        let top_level_scope = Rc::new(top_level_scope);
        let scope = Scope::new(Some(Rc::clone(&top_level_scope)));
        let scope = parse_body(scope, assignments);
        print_assignments(&scope)
    }

    #[test]
    fn test_independent_assignments() {
        k9::snapshot!(
            test_body(vec![assign("foo", "1 + 2"), assign("bar", "3 + 4")]),
            "
foo (n) = (+ 1 2)
bar (n) = (+ 3 4)
"
        );
    }

    #[test]
    fn test_shadowing() {
        k9::snapshot!(
            test_body(vec![assign("foo", "1"), assign("foo", "2")]),
            "
foo (n) = 1
foo_1 (n) = 2
"
        );
    }

    #[test]
    fn test_backreference() {
        k9::snapshot!(
            test_body(vec![assign("foo", "1"), assign("bar", "foo + 1")]),
            "
foo (n) = 1
bar (n) = (+ foo 1)
"
        );

        k9::snapshot!(
            test_body(vec![
                assign("foo", "1"),
                assign("foo", "foo + 1"),
                assign("foo", "foo + 1")
            ]),
            "
foo (n) = 1
foo_1 (n) = (+ foo 1)
foo_2 (n) = (+ foo_1 1)
"
        );
    }

    #[test]
    fn test_recursive_reference() {
        k9::snapshot!(
            test_body(vec![assign("foo", "foo + 1")]),
            "foo depends on foo"
        );
    }

    #[test]
    fn test_error_propagation() {
        k9::snapshot!(
            test_body(vec![assign("foo", "[+]"), assign("bar", "foo + 1")]),
            "
foo failed: ArrayLiteralNotNoun
bar depends on failed foo
"
        );
    }

    #[test]
    fn test_cyclic_reference() {
        k9::snapshot!(
            test_body(vec![assign("foo", "bar + 1"), assign("bar", "foo + 1")]),
            "
foo depends on bar
bar depends on foo
"
        );

        k9::snapshot!(
            test_body(vec![
                assign("foo", "bar + 1"),
                assign("bar", "baz + 1"),
                assign("baz", "foo + 1")
            ]),
            "
foo depends on bar
bar depends on baz
baz depends on foo
"
        );
    }

    #[test]
    fn test_forward_reference() {
        k9::snapshot!(
            test_body(vec![assign("foo", "bar + 1"), assign("bar", "1")]),
            "
foo (n) = (+ bar 1)
bar (n) = 1
"
        );
    }
}