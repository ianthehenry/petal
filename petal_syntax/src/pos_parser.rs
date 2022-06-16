use crate::expression::{Builtin, Expression, Identifier, RichIdentifier};
use crate::terms::Term;
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

type Statement = crate::statement::Statement<Term>;

#[derive(Debug, Clone, Copy)]
enum Direction {
    Forwards,
    Backwards,
}
use Direction::*;

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

#[derive(Debug)]
pub enum ParseError {
    DidNotFullyReduce(Vec<(Expression, PartOfSpeech)>),
    ArrayLiteralNotNoun,
    BadReference(Identifier),
    SubAssignmentFailed,
    CyclicAssignments,
    BlockWithoutResult,
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

#[derive(Debug)]
struct ParseFrame {
    stack: Vec<Option<(Expression, PartOfSpeech)>>,
    input: Vec<Term>,
    end_reached: bool,
    finish: fn(Expression, PartOfSpeech) -> Result<Expression, ParseError>,
}

impl ParseFrame {
    fn new(
        input: Vec<Term>,
        finish: fn(Expression, PartOfSpeech) -> Result<Expression, ParseError>,
    ) -> Self {
        Self {
            input,
            end_reached: false,
            stack: vec![None, None, None, None],
            finish,
        }
    }
}

// PollingName: if you have the name, give it to me. If you don't, resume me
// anyway.
//
// PendingName: don't resume me until you have the name.
//
// We could do away with PollingName if we could synchronously check the parent
// scope to see if an identifier is defined. However, this requires having a
// reference to the parent scope, which complicated ownership stuff. We'd
// actually need like an Rc<RefCell<BlockParsnip>>, and then the parsnip trait
// would only be implementable on Rc<RefCell<BlockParsnip>>, and that's all
// gross. But maybe worth it? The inverted control flow that PollingName
// begets is really pretty complicated.
enum ParseResult {
    Complete(Expression, PartOfSpeech),
    PollingName(String),
    PendingName(String),
    PendingId(Identifier),
}

fn identity(expr: Expression, _: PartOfSpeech) -> Result<Expression, ParseError> {
    Ok(expr)
}

fn wrap_parens(expr: Expression, _: PartOfSpeech) -> Result<Expression, ParseError> {
    Ok(Expression::Parens(Box::new(expr)))
}

fn wrap_brackets(expr: Expression, pos: PartOfSpeech) -> Result<Expression, ParseError> {
    match pos {
        Noun => {
            let exprs = match expr {
                Expression::Tuple(exprs) => exprs,
                expr => vec![expr],
            };
            Ok(Expression::Brackets(exprs))
        }
        _ => Err(ParseError::ArrayLiteralNotNoun),
    }
}

fn pop_expr(stack: &mut Vec<Option<(Expression, PartOfSpeech)>>) -> Expression {
    let (expr, _) = stack.pop().unwrap().unwrap();
    expr
}

fn pop_adverb(stack: &mut Vec<Option<(Expression, PartOfSpeech)>>) -> (Expression, Arity) {
    let (expr, pos) = stack.pop().unwrap().unwrap();
    match pos {
        Adverb(_, result_arity) => (expr, result_arity),
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
        let lhs = pop_expr($stack);
        let rhs = pop_expr($stack);
        $stack.push(Some((
            Expression::binary(Expression::Implicit($inner), lhs, rhs),
            $pos,
        )));
    };
}

macro_rules! bin_impl_rl {
    ($stack:ident, $inner:expr, $pos:expr) => {
        let rhs = pop_expr($stack);
        let lhs = pop_expr($stack);
        $stack.push(Some((
            Expression::binary(Expression::Implicit($inner), lhs, rhs),
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

fn reduce_stack(stack: &mut Vec<Option<(Expression, PartOfSpeech)>>) {
    use Arity::*;

    loop {
        match &stack[stack.len() - 4..] {
            stack![a1, v] => {
                let (adverb, result_arity) = pop_adverb(stack);
                let verb = pop_expr(stack);
                stack.push(Some((Expression::unary(adverb, verb), Verb(result_arity))));
            }

            stack![svn, v1, n] => lookahead!(stack, {
                let verb = pop_expr(stack);
                let noun = pop_expr(stack);
                stack.push(Some((Expression::unary(verb, noun), Noun)));
            }),

            stack![_, vn, a2, vn] => lookahead!(stack, {
                let lhs = pop_expr(stack);
                let (conjunction, result_arity) = pop_adverb(stack);
                let rhs = pop_expr(stack);
                stack.push(Some((
                    Expression::binary(conjunction, lhs, rhs),
                    Verb(result_arity),
                )));
            }),

            stack![sv, n, v2, n] => lookahead!(stack, {
                let lhs = pop_expr(stack);
                let verb = pop_expr(stack);
                let rhs = pop_expr(stack);
                stack.push(Some((Expression::binary(verb, lhs, rhs), Noun)));
            }),

            stack![svn, n, n] => lookahead!(stack, {
                let first = pop_expr(stack);
                let second = pop_expr(stack);

                let result = match second {
                    Expression::Tuple(mut exprs) => {
                        exprs.push(first);
                        Expression::Tuple(exprs)
                    }
                    _ => Expression::Tuple(vec![second, first]),
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

pub(super) fn just_parse(
    statements: Vec<Statement>,
) -> Result<(Expression, PartOfSpeech), ParseError> {
    let allocator = Rc::new(RefCell::new(Allocator::new()));
    match BlockParsnip::new(allocator, statements).parse()? {
        ParseResult::Complete(expr, pos) => Ok((expr, pos)),
        ParseResult::PendingName(_) | ParseResult::PollingName(_) | ParseResult::PendingId(_) => {
            panic!("partial parse")
        }
    }
}

// A "parsnip" is a parsing computation that can be suspended and resumed. A
// better name might be something with "fiber" in it, but that's not as fun.
trait Parsnip {
    fn not_yet_known(&mut self, name: &String);
    fn provide(&mut self, id: RichIdentifier, pos: PartOfSpeech);
    fn parse(&mut self) -> Result<ParseResult, ParseError>;
}

struct ExpressionParsnip(Vec<ParseFrame>);

impl ExpressionParsnip {
    fn new(terms: Vec<Term>) -> Self {
        ExpressionParsnip(vec![ParseFrame::new(terms, identity)])
    }
}

impl Parsnip for ExpressionParsnip {
    fn not_yet_known(&mut self, _name: &String) {
        panic!("expressions never poll")
    }

    fn provide(&mut self, id: RichIdentifier, pos: PartOfSpeech) {
        let top_frame = self.0.last_mut().unwrap();
        top_frame.stack.push(Some((Expression::id(id), pos)));
    }

    fn parse(&mut self) -> Result<ParseResult, ParseError> {
        let call_stack = &mut self.0;
        loop {
            let frame = call_stack.last_mut().unwrap();

            reduce_stack(&mut frame.stack);

            match frame.input.pop() {
                None => {
                    if frame.end_reached {
                        let frame = call_stack.pop().unwrap();
                        let without_sentinels =
                            frame.stack.into_iter().flatten().collect::<Vec<_>>();
                        let (expr, pos) = match without_sentinels.len() {
                            0 => Ok((Expression::Tuple(vec![]), Noun)),
                            1 => Ok(without_sentinels.into_iter().next().unwrap()),
                            _ => Err(ParseError::DidNotFullyReduce(without_sentinels)),
                        }?;
                        let expr = (frame.finish)(expr, pos)?;

                        match call_stack.last_mut() {
                            None => return Ok(ParseResult::Complete(expr, pos)),
                            Some(next) => next.stack.push(Some((expr, pos))),
                        }
                    } else {
                        frame.end_reached = true;
                        frame.stack.push(None);
                    }
                }

                Some(term) => match term {
                    Term::NumericLiteral(num) => {
                        frame.stack.push(Some((Expression::num(num), Noun)))
                    }
                    Term::Coefficient(num) => frame.stack.push(Some((
                        Expression::unary(
                            Expression::Implicit(Builtin::Scale),
                            Expression::num(num),
                        ),
                        Verb(Arity::Unary),
                    ))),
                    Term::Identifier(id) => return Ok(ParseResult::PendingName(id)),
                    Term::Parens(terms) => call_stack.push(ParseFrame::new(terms, wrap_parens)),
                    Term::Brackets(terms) => call_stack.push(ParseFrame::new(terms, wrap_brackets)),
                },
            };
        }
    }
}

struct ParseOperation {
    id: Identifier,
    state: Box<dyn Parsnip>,
}

impl ParseOperation {
    fn new(id: Identifier, state: Box<dyn Parsnip>) -> Self {
        ParseOperation { id, state }
    }
}

struct BlockParsnip {
    name_to_ids: HashMap<String, Vec<Identifier>>,
    id_to_name: HashMap<Identifier, String>,

    polling_name: HashMap<String, Vec<ParseOperation>>,
    blocked_on_name: HashMap<String, Vec<ParseOperation>>,
    blocked_on_id: HashMap<Identifier, Vec<ParseOperation>>,
    complete: HashMap<Identifier, (Expression, PartOfSpeech)>,
    failed: HashMap<Identifier, ParseError>,
    unblocked: Vec<ParseOperation>,

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
    Complete(Identifier, &'a Expression, PartOfSpeech),
}

impl BlockParsnip {
    fn new(allocator: Rc<RefCell<Allocator>>, statements: Vec<Statement>) -> Self {
        let mut this = BlockParsnip {
            name_to_ids: HashMap::new(),
            id_to_name: HashMap::new(),
            allocator,
            polling_name: HashMap::new(),
            blocked_on_name: HashMap::new(),
            blocked_on_id: HashMap::new(),
            complete: HashMap::new(),
            failed: HashMap::new(),
            unblocked: vec![],
        };

        for statement in statements {
            this.begin(statement);
        }
        // We need to begin elements from top-to-bottom, but every time we begin
        // something we push it onto a stack. But I think it will be more
        // efficient to parse from top-to-bottom as well, as I expect
        // backreferences will be more common than forward references. This
        // reverse should not alter the semantics or result of the parse in any
        // way.
        // TODO: is this actually better? Might be worth profiling when I have a
        // nontrivial program to test it on.
        this.unblocked.reverse();
        this
    }

    fn begin(&mut self, statement: Statement) {
        match statement {
            Statement::SimpleAssignment(name, terms) => {
                let id = self.learn_name(name);
                self.unblocked.push(ParseOperation::new(
                    id,
                    Box::new(ExpressionParsnip::new(terms)),
                ));
            }
            Statement::CompoundAssignment(name, statements) => {
                let id = self.learn_name(name);
                self.unblocked.push(ParseOperation::new(
                    id,
                    Box::new(BlockParsnip::new(Rc::clone(&self.allocator), statements)),
                ));
            }
            Statement::Expression(terms) => {
                // TODO: another case where we could reference a constant or something
                self.begin(Statement::SimpleAssignment("_".to_string(), terms))
            }
        }
    }

    fn polling_name(&mut self, prereq_name: String, parse: ParseOperation) {
        self.polling_name
            .entry(prereq_name)
            .or_insert_with(Vec::new)
            .push(parse);
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
        panic!("identifier not found");
    }

    fn complete(&mut self, id: Identifier, expr: Expression, pos: PartOfSpeech) {
        let rich_id = RichIdentifier::new(id, self.name_of_id(&id));
        if let Some(parses) = self.blocked_on_id.remove(&id) {
            for mut parse in parses {
                parse.state.provide(rich_id.clone(), pos);
                self.unblocked.push(parse);
            }
        }
        assert!(self.complete.insert(id, (expr, pos)).is_none());
    }

    fn lookup_previous_identifier(&self, name: &str, as_of: Identifier) -> Option<Identifier> {
        self.name_to_ids.get(name).and_then(|bindings| {
            bindings
                .iter()
                .filter(|id| **id < as_of)
                .map(Identifier::clone)
                .last()
        })
    }

    fn lookup_next_identifier(&self, name: &str, as_of: Identifier) -> Option<Identifier> {
        self.name_to_ids.get(name).and_then(|bindings| {
            bindings
                .iter()
                .filter(|id| **id >= as_of)
                .map(Identifier::clone)
                .next()
        })
    }

    fn lookup_by_id(&self, id: Identifier) -> LookupResult {
        if let Some((expr, pos)) = self.complete.get(&id) {
            return LookupResult::Complete(id, expr, *pos);
        }
        if let Some(error) = self.failed.get(&id) {
            return LookupResult::Failed(id, error);
        }
        // a more "obvious" approach would be to check the two "blocked" keys
        // for the Pending result and then panic if we never find something. but
        // that would require either linearly scanning the blocked dictionaries
        // or storing an extra map. so we're taking advantage of the invariant
        // that we only have Identifiers for names that are pending
        LookupResult::Pending(id)
    }

    // TODO: this is stupidly O(number of definitions), but because everything
    // is sorted, it could easily be O(log(number of definitions)).
    fn lookup_identifier(
        &self,
        name: &str,
        as_of: Identifier,
        dir: Direction,
    ) -> Option<Identifier> {
        match dir {
            Backwards => self.lookup_previous_identifier(name, as_of),
            Forwards => self.lookup_next_identifier(name, as_of),
        }
    }

    fn lookup(&self, name: &str, as_of: Identifier, dir: Direction) -> LookupResult {
        match self.lookup_identifier(name, as_of, dir) {
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

    // returns the ParseOperation if it does not handle it
    fn try_providing_name(
        &mut self,
        mut op: ParseOperation,
        name: &String,
        dir: Direction,
    ) -> Option<ParseOperation> {
        match self.lookup(&name, op.id, dir) {
            // TODO: should add support for "not yet parsed but part of
            // speech already known"
            LookupResult::Unknown => Some(op),
            LookupResult::Pending(prereq_id) => {
                self.blocked_on_id(prereq_id, op);
                None
            }
            LookupResult::Failed(prereq_id, _) => {
                self.failed(op.id, ParseError::BadReference(prereq_id));
                None
            }
            LookupResult::Complete(prereq_id, _expr, pos) => {
                op.state
                    .provide(RichIdentifier::new(prereq_id, name.clone()), pos);
                self.unblocked.push(op);
                None
            }
        }
    }
}

impl Parsnip for BlockParsnip {
    fn not_yet_known(&mut self, name: &String) {
        // Now that we know that it's not present in the parent, we can check
        // for forward bindings. We only do this for pending, not polling,
        // because polling operations need to resume to themselves first -- they
        // shouldn't look forward in the outer scope until they're sure that
        // they don't have the value in their inner scope.

        // TODO: It's dumb that we do the lookup once for everyone waiting on
        // this, instead of looking it up once and providing it to everyone.
        for op in self.blocked_on_name.remove(name).into_iter().flatten() {
            if let Some(op) = self.try_providing_name(op, name, Forwards) {
                self.blocked_on_name(name.clone(), op);
            }
        }

        // TODO: does this need to be a hashmap at all? can't it only ever have
        // one element?
        for mut op in self.polling_name.remove(name).into_iter().flatten() {
            op.state.not_yet_known(name);
            self.unblocked.push(op);
        }
    }

    fn provide(&mut self, id: RichIdentifier, pos: PartOfSpeech) {
        if let Some(parses) = self.polling_name.remove(&id.name) {
            for mut parse in parses {
                parse.state.provide(id.clone(), pos);
                self.unblocked.push(parse);
            }
        }

        if let Some(parses) = self.blocked_on_name.remove(&id.name) {
            for mut parse in parses {
                parse.state.provide(id.clone(), pos);
                self.unblocked.push(parse);
            }
        }

        if let Some(parses) = self.blocked_on_id.remove(&id.id) {
            for mut parse in parses {
                parse.state.provide(id.clone(), pos);
                self.unblocked.push(parse);
            }
        }
    }

    fn parse(&mut self) -> Result<ParseResult, ParseError> {
        assert!(self.polling_name.is_empty());

        while let Some(mut op) = self.unblocked.pop() {
            match op.state.parse() {
                Err(e) => {
                    self.failed(op.id, e);
                }
                Ok(ParseResult::Complete(expr, pos)) => {
                    self.complete(op.id, expr, pos);
                }
                Ok(ParseResult::PendingId(prereq_id)) => {
                    self.blocked_on_id(prereq_id, op);
                }
                Ok(ParseResult::PendingName(prereq_name)) => {
                    if let Some(op) = self.try_providing_name(op, &prereq_name, Backwards) {
                        self.blocked_on_name(prereq_name.clone(), op);
                        return Ok(ParseResult::PollingName(prereq_name));
                    }
                }
                Ok(ParseResult::PollingName(prereq_name)) => {
                    if let Some(op) = self.try_providing_name(op, &prereq_name, Backwards) {
                        self.polling_name(prereq_name.clone(), op);
                        return Ok(ParseResult::PollingName(prereq_name));
                    }
                }
            }
        }
        // At this point we have fully reduced ourselves.
        //
        // If any assignment failed, the whole parse failed.
        //
        // Otherwise, if something is blocked on name, we need to return pending.
        //
        // Otherwise, if something is blocked on an ID defined in a parent scope,
        // then we need to return pending.
        //
        // Otherwise, if something is blocked on an ID defined in *my* scope,
        // there's a cyclic definition and we can error immediately.
        //
        // Otherwise, we successfully parsed every assignment.

        if !self.failed.is_empty() {
            return Err(ParseError::SubAssignmentFailed);
        }

        if let Some(name) = self.blocked_on_name.keys().next() {
            return Ok(ParseResult::PendingName(name.clone()));
        }

        // TODO: a bit of denormalization would remove the need for a linear
        // scan here
        for id in self.blocked_on_id.keys() {
            if !self.id_to_name.contains_key(id) {
                return Ok(ParseResult::PendingId(*id));
            }
        }
        if !self.blocked_on_id.is_empty() {
            return Err(ParseError::CyclicAssignments);
        }

        // TODO: in order to use this to drive a repl, we need to have some way
        // to prevent checking for completes. I think the right approach is to
        // make this return a "I am ready to be done" status, and have the
        // caller decide to invoke some kind of "finalize" method to extract the
        // actual expression/POS at its discretion.

        // TODO: should maybe cache this key? also do we want to allow multiple
        // top-level statements...? is that actually desirable in any way?
        match self.name_to_ids.get("_") {
            None => Err(ParseError::BlockWithoutResult),
            Some(ids) => {
                // NOTE: Before I was using traits, the parse function actually
                // moved the Parsnip value and ParseResult returned it back (or
                // didn't). But I can't figure out how to do that in a way that
                // is object-safe, and the trait approach seems otherwise
                // superior to a variant. So this mutates itself until its in
                // sort of an invalid state -- bad things would happen if the
                // caller continued to use the parsnip after this.
                let (result_expr, result_pos) = self.complete.remove(ids.last().unwrap()).unwrap();
                let assignments = self
                    .complete
                    .drain()
                    .map(|(id, (expr, _pos))| {
                        let name = self.id_to_name.remove(&id).unwrap();
                        (RichIdentifier::new(id, name), expr)
                    })
                    .collect::<HashMap<_, _>>();

                Ok(ParseResult::Complete(
                    Expression::Compound(assignments, Box::new(result_expr.clone())),
                    result_pos,
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn show_annotated_expr(annotated_expr: &(Expression, PartOfSpeech)) -> String {
        let (expr, pos) = annotated_expr;
        format!("{}:{}", pos, expr)
    }

    fn show_annotated_exprs(annotated_exprs: Vec<(Expression, PartOfSpeech)>) -> String {
        annotated_exprs
            .iter()
            .map(show_annotated_expr)
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn parse_to_completion(input: Vec<Term>) -> Result<(Expression, PartOfSpeech), ParseError> {
        let mut call_stack = ExpressionParsnip::new(input);

        loop {
            match call_stack.parse()? {
                ParseResult::Complete(expr, pos) => return Ok((expr, pos)),
                ParseResult::PendingId(_) => todo!(),
                ParseResult::PollingName(_) => todo!(),
                ParseResult::PendingName(name) => {
                    let pos = match name.as_str() {
                        "+" | "*" => Verb(Arity::Binary),
                        "neg" | "sign" => Verb(Arity::Unary),
                        "." => Adverb(Arity::Binary, Arity::Binary),
                        "fold" => Adverb(Arity::Unary, Arity::Unary),
                        "flip" => Adverb(Arity::Unary, Arity::Binary),
                        "x" | "y" => Noun,
                        _ => panic!("unknown identifier"),
                    };
                    call_stack.provide(RichIdentifier::new(0, name), pos);
                }
            }
        }
    }

    fn preparse(input: &str) -> Vec<Term> {
        let tokens = crate::tokenizer::tokenize(input);
        let terms = crate::statement_parser::parse_expression(tokens).unwrap();
        let terms = crate::semicolons::resolve_expression(terms);
        let terms = crate::op_splitter::split_expression(terms);
        crate::coefficient_grouper::group(terms)
    }

    fn test(input: &str) -> String {
        match parse_to_completion(preparse(input)) {
            Ok(expr) => show_annotated_expr(&expr),
            Err(ParseError::DidNotFullyReduce(exprs)) => {
                format!("incomplete parse: {}", show_annotated_exprs(exprs))
            }
            Err(error) => format!("error: {:?}", error),
        }
    }

    fn begin_parse(input: &str) -> ExpressionParsnip {
        ExpressionParsnip::new(preparse(input))
    }

    fn advance(call_stack: &mut ExpressionParsnip) -> String {
        match call_stack.parse() {
            Ok(ParseResult::Complete(expr, pos)) => show_annotated_expr(&(expr, pos)),
            Ok(ParseResult::PendingName(id)) => format!("awaiting {}", id),
            Ok(ParseResult::PollingName(_)) => todo!(),
            Ok(ParseResult::PendingId(_)) => todo!(),
            Err(ParseError::DidNotFullyReduce(exprs)) => {
                format!("incomplete parse: {}", show_annotated_exprs(exprs))
            }
            Err(error) => format!("error: {:?}", error),
        }
    }

    #[test]
    fn test_parser() {
        k9::snapshot!(test("neg 1 + 2"), "n:(neg (+ 1 2))");
        k9::snapshot!(test("fold +"), "v1:(fold +)");
        k9::snapshot!(test("fold + x"), "n:((fold +) x)");
        k9::snapshot!(test("x + y"), "n:(+ x y)");
        k9::snapshot!(test("x +.* y"), "n:((. + *) x y)");
        k9::snapshot!(test("x fold + . * y"), "n:((. (fold +) *) x y)");
        k9::snapshot!(test("x + . fold * y"), "n:((. + (fold *)) x y)");
        k9::snapshot!(test("x fold + . fold * y"), "n:((. (fold +) (fold *)) x y)");
        k9::snapshot!(
            test("x fold * . fold + . fold * y"),
            "n:((. (fold *) (. (fold +) (fold *))) x y)"
        );
    }

    #[test]
    fn test_adverbs() {
        k9::snapshot!(test("1 + 2"), "n:(+ 1 2)");
        k9::snapshot!(test("1 flip + 2"), "n:((flip +) 1 2)");
    }

    #[test]
    fn test_tuples() {
        k9::snapshot!(test("1 + 1 2"), "n:(+ 1 (<tuple> 1 2))");
        k9::snapshot!(test("1 + 1 2 3 4 5"), "n:(+ 1 (<tuple> 1 2 3 4 5))");
        k9::snapshot!(test("1 + 1 neg 2"), "n:(+ 1 (<tuple> 1 (neg 2)))");
        k9::snapshot!(test("1 2 + 1 2"), "n:(+ (<tuple> 1 2) (<tuple> 1 2))");
        k9::snapshot!(test("1 + (1 2) 3"), "n:(+ 1 (<tuple> (<tuple> 1 2) 3))");
        k9::snapshot!(test("1 + (1 2 3)"), "n:(+ 1 (<tuple> 1 2 3))");
        k9::snapshot!(test("1 + 2; 3"), "n:(<tuple> (+ 1 2) 3)");
    }

    #[test]
    fn test_operator_sections() {
        k9::snapshot!(test("+ 1"), "v1:(<rhs> + 1)");
        k9::snapshot!(test("+ 1 2"), "v1:(<rhs> + (<tuple> 1 2))");
        k9::snapshot!(test("(+ 1) 2"), "n:((<rhs> + 1) 2)");
        k9::snapshot!(test("1 +"), "v1:(<lhs> + 1)");
        k9::snapshot!(test("1 2 +"), "v1:(<lhs> + (<tuple> 1 2))");

        k9::snapshot!(test("flip + 1"), "v1:(<rhs> (flip +) 1)");
        k9::snapshot!(test("flip + 1 2"), "v1:(<rhs> (flip +) (<tuple> 1 2))");
        k9::snapshot!(test("(flip + 1) 2"), "n:((<rhs> (flip +) 1) 2)");
        k9::snapshot!(test("1 flip +"), "v1:(<lhs> (flip +) 1)");
        k9::snapshot!(test("1 2 flip +"), "v1:(<lhs> (flip +) (<tuple> 1 2))");
    }

    #[test]
    fn test_unary_composition() {
        k9::snapshot!(test("neg sign"), "v1:(<comp> neg sign)");
        k9::snapshot!(test("neg sign neg"), "v1:(<comp> neg (<comp> sign neg))");

        k9::snapshot!(test("fold + fold *"), "v1:(<comp> (fold +) (fold *))");
        k9::snapshot!(
            test("fold + fold * fold +"),
            "v1:(<comp> (fold +) (<comp> (fold *) (fold +)))"
        );
    }

    #[test]
    fn test_implicit_composition() {
        k9::snapshot!(
            test("neg + sign"),
            "v2:(<comp-lhs> (<comp-rhs> + sign) neg)"
        );
        k9::snapshot!(
            test("neg sign + neg"),
            "v2:(<comp-lhs> (<comp-lhs> (<comp-rhs> + neg) sign) neg)"
        );
        k9::snapshot!(
            test("neg + sign neg"),
            "v2:(<comp-lhs> (<comp-rhs> + (<comp> sign neg)) neg)"
        );

        k9::snapshot!(
            test("neg + (sign neg)"),
            "v2:(<comp-lhs> (<comp-rhs> + (<comp> sign neg)) neg)"
        );

        k9::snapshot!(test("+ neg"), "v2:(<comp-rhs> + neg)");
        k9::snapshot!(test("neg +"), "v2:(<comp-lhs> + neg)");
        k9::snapshot!(test("neg + 1"), "v1:(<comp> neg (<rhs> + 1))");

        k9::snapshot!(test("flip + neg"), "v2:(<comp-rhs> (flip +) neg)");
        k9::snapshot!(test("neg flip +"), "v2:(<comp-lhs> (flip +) neg)");
        k9::snapshot!(test("neg flip + 1"), "v1:(<comp> neg (<rhs> (flip +) 1))");
    }

    #[test]
    fn test_array_literals() {
        k9::snapshot!(test("[]"), "n:[]");
        k9::snapshot!(test("[1 2 3]"), "n:[1 2 3]");
        k9::snapshot!(test("[1 2 3; 4 5 6]"), "n:[[1 2 3] [4 5 6]]");
        k9::snapshot!(
            test("[1 2 3; 4 5 6;; 7 8 9; 10 11 12]"),
            "n:[[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]]"
        );
    }

    #[test]
    fn test_confusing_expressions() {
        k9::snapshot!(test("* 1 +"), "v2:(<comp-rhs> * (<lhs> + 1))");
        k9::snapshot!(test("* + 1"), "v2:(<comp-rhs> * (<rhs> + 1))");
        k9::snapshot!(test("1 * +"), "v2:(<comp-lhs> + (<lhs> * 1))");
    }

    #[test]
    fn test_implicit_equivalences() {
        k9::snapshot!(
            test("neg + sign"),
            "v2:(<comp-lhs> (<comp-rhs> + sign) neg)"
        );
        k9::snapshot!(
            test("neg (+ sign)"),
            "v2:(<comp-lhs> (<comp-rhs> + sign) neg)"
        );

        k9::snapshot!(test("neg + 1"), "v1:(<comp> neg (<rhs> + 1))");
        k9::snapshot!(test("neg (+ 1)"), "v1:(<comp> neg (<rhs> + 1))");

        k9::snapshot!(test("+ sign neg"), "v2:(<comp-rhs> + (<comp> sign neg))");
        k9::snapshot!(test("+ (sign neg)"), "v2:(<comp-rhs> + (<comp> sign neg))");
    }

    #[test]
    fn test_parse_errors() {
        k9::snapshot!(test("* +"), "incomplete parse: v2:+ v2:*");
        k9::snapshot!(test("* flip +"), "incomplete parse: v2:(flip +) v2:*");
        k9::snapshot!(test(". +"), "incomplete parse: v2:+ a2:.");
        k9::snapshot!(test("+ ."), "incomplete parse: a2:. v2:+");
        k9::snapshot!(test("flip ."), "incomplete parse: a2:. a1:flip");
        k9::snapshot!(test("fold ."), "incomplete parse: a2:. a1:fold");
        k9::snapshot!(test(". flip"), "incomplete parse: a1:flip a2:.");
        k9::snapshot!(test(". fold"), "incomplete parse: a1:fold a2:.");
        k9::snapshot!(test("flip fold"), "incomplete parse: a1:fold a1:flip");
    }

    #[test]
    fn test_partial_parsing() {
        fn id(name: &str) -> RichIdentifier {
            RichIdentifier::new(0, name.to_string())
        }
        let mut call_stack = begin_parse("x + foo");
        k9::snapshot!(advance(&mut call_stack), "awaiting foo");
        call_stack.provide(id("foo"), Noun);
        k9::snapshot!(advance(&mut call_stack), "awaiting +");
        call_stack.provide(id("+"), Verb(Arity::Binary));
        k9::snapshot!(advance(&mut call_stack), "awaiting x");
        call_stack.provide(id("x"), Noun);
        k9::snapshot!(advance(&mut call_stack), "n:(+ x foo)");
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
            if self.name_seen_at.get(&rich_id).is_none() {
                let ix = self.name_indices.entry(rich_id.name.clone()).or_insert(0);
                self.name_seen_at.insert(rich_id, *ix);
                *ix = *ix + 1;
            }
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
        Complete(&'a Expression, &'a PartOfSpeech),
        Failed(&'a ParseError),
        Cyclic(&'a Identifier),
        Pending(&'a str),
    }

    fn rewrite_ids<F: FnMut(&RichIdentifier) -> RichIdentifier>(
        expr: &Expression,
        f: &mut F,
    ) -> Expression {
        use Expression::*;

        match expr {
            Atom(crate::expression::Atom::Identifier(rich_id)) => {
                Atom(crate::expression::Atom::Identifier(f(rich_id)))
            }
            Atom(_) => expr.clone(),
            Parens(exprs) => Parens(Box::new(rewrite_ids(exprs, f))),
            Implicit(x) => Implicit(*x),
            Tuple(exprs) => Tuple(exprs.iter().map(|expr| rewrite_ids(expr, f)).collect()),
            Brackets(exprs) => Brackets(exprs.iter().map(|expr| rewrite_ids(expr, f)).collect()),
            UnaryApplication(expr1, expr2) => {
                Expression::unary(rewrite_ids(expr1, f), rewrite_ids(expr2, f))
            }
            BinaryApplication(expr1, expr2, expr3) => Expression::binary(
                rewrite_ids(expr1, f),
                rewrite_ids(expr2, f),
                rewrite_ids(expr3, f),
            ),
            Compound(bindings, expr) => Expression::Compound(
                bindings
                    .iter()
                    .map(|(id, expr)| (f(id), rewrite_ids(expr, f)))
                    .collect(),
                Box::new(rewrite_ids(expr, f)),
            ),
        }
    }

    fn print_assignments(block: &BlockParsnip) -> String {
        let mut disambiguator = Disambiguator::new();

        let completes = block
            .complete
            .iter()
            .map(|(id, (expr, pos))| (*id, AssignmentStatus::Complete(expr, pos)));
        let failures = block
            .failed
            .iter()
            .map(|(id, error)| (*id, AssignmentStatus::Failed(error)));
        let cyclics = block.blocked_on_id.iter().flat_map(|(missing_id, parses)| {
            parses
                .iter()
                .map(|parse| (parse.id, AssignmentStatus::Cyclic(missing_id)))
        });
        let pendings = block
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
            let name = block.name_of_id(&id);
            let rich_id = RichIdentifier::new(id, name);
            disambiguator.see(rich_id.clone());

            if first {
                first = false;
            } else {
                result.push('\n');
            }

            match status {
                AssignmentStatus::Complete(expr, pos) => {
                    let mut f = |rich_id: &RichIdentifier| {
                        disambiguator.see(rich_id.clone());
                        RichIdentifier::new(rich_id.id, disambiguator.view(rich_id))
                    };
                    let expr = rewrite_ids(&expr, &mut f);

                    result.push_str(&format!(
                        "{} ({}) = {}",
                        disambiguator.view(&rich_id),
                        pos,
                        expr
                    ));
                }
                AssignmentStatus::Failed(ParseError::BadReference(prereq_id)) => {
                    let prereq_name = block.name_of_id(prereq_id);
                    let rich_prereq_id = RichIdentifier::new(*prereq_id, prereq_name);
                    disambiguator.see(rich_prereq_id.clone());
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
                    let prereq_name = block.name_of_id(prereq_id);
                    let rich_prereq_id = RichIdentifier::new(*prereq_id, prereq_name);
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

    fn lookup_builtin(name: &String) -> Option<PartOfSpeech> {
        match name.as_str() {
            "+" | "*" => Some(Verb(Arity::Binary)),
            "neg" | "sign" => Some(Verb(Arity::Unary)),
            "." => Some(Adverb(Arity::Binary, Arity::Binary)),
            "fold" => Some(Adverb(Arity::Unary, Arity::Unary)),
            "flip" => Some(Adverb(Arity::Unary, Arity::Binary)),
            _ => None,
        }
    }

    // TODO: this is kinda duplicated with parse_to_completion
    fn test_body(input: &str) -> String {
        let tokens = crate::tokenizer::tokenize(input);
        let statements = crate::statement_parser::parse_tokens(tokens).unwrap();
        let statements = crate::semicolons::rewrite(statements);
        let statements = crate::op_splitter::rewrite(statements);
        let statements = crate::coefficient_grouper::rewrite(statements);

        let allocator = Rc::new(RefCell::new(Allocator::new()));
        let mut block = BlockParsnip::new(allocator, statements);

        loop {
            match block.parse() {
                Err(_) => break,
                Ok(ParseResult::Complete(_, _)) => break,
                Ok(ParseResult::PendingId(_)) => break,
                Ok(ParseResult::PendingName(_)) => break,
                Ok(ParseResult::PollingName(name)) => {
                    if let Some(pos) = lookup_builtin(&name) {
                        block.provide(RichIdentifier::new(0, name), pos);
                    } else {
                        block.not_yet_known(&name);
                    }
                }
            }
        }
        print_assignments(&block)
    }

    #[test]
    fn test_independent_assignments() {
        k9::snapshot!(
            test_body(
                "
foo = 1 + 2
bar = 3 + 4"
            ),
            "
foo (n) = (+ 1 2)
bar (n) = (+ 3 4)
"
        );
    }

    #[test]
    fn test_shadowing() {
        k9::snapshot!(
            test_body(
                "
foo = 1
foo = 2"
            ),
            "
foo (n) = 1
foo_1 (n) = 2
"
        );
    }

    #[test]
    fn test_backreference() {
        k9::snapshot!(
            test_body(
                "
foo = 1
bar = foo + 1"
            ),
            "
foo (n) = 1
bar (n) = (+ foo 1)
"
        );

        k9::snapshot!(
            test_body(
                "
foo = 1
foo = foo + 1
foo = foo + 1
"
            ),
            "
foo (n) = 1
foo_1 (n) = (+ foo 1)
foo_2 (n) = (+ foo_1 1)
"
        );
    }

    #[test]
    fn test_recursive_reference() {
        k9::snapshot!(test_body("foo = foo + 1"), "foo depends on foo");
    }

    #[test]
    fn test_error_propagation() {
        k9::snapshot!(
            test_body(
                "
foo = [+]
bar = foo + 1
"
            ),
            "
foo failed: ArrayLiteralNotNoun
bar depends on failed foo
"
        );
    }

    #[test]
    fn test_cyclic_reference() {
        k9::snapshot!(
            test_body(
                "
foo = bar + 1
bar = foo + 1
"
            ),
            "
foo depends on bar
bar depends on foo
"
        );

        k9::snapshot!(
            test_body(
                "
foo = bar + 1
bar = baz + 1
baz = foo + 1
"
            ),
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
            test_body(
                "
foo = bar + 1
bar = 1"
            ),
            "
foo (n) = (+ bar 1)
bar (n) = 1
"
        );
    }

    #[test]
    fn test_nested_blocks() {
        k9::snapshot!(
            test_body(
                "
foo =
  x =
    y = 10
    z = 20
    y + z
  x
"
            ),
            "foo (n) = (let ((x (let ((y 10) (z 20)) (+ y z)))) x)"
        );
    }

    #[test]
    fn test_block_reads_from_outer_scope() {
        k9::snapshot!(
            test_body(
                "
foo = 10
bar = x
  x = y
    y = foo
"
            ),
            "
foo (n) = 10
bar (n) = (let ((x (let ((y foo)) y))) x)
"
        );

        // TODO: this is wrong!
        k9::snapshot!(
            test_body(
                "
foo = 10
bar = x
  x = foo
    foo = 20
"
            ),
            "
foo (n) = 10
bar (n) = (let ((x (let ((foo_1 20)) foo))) x)
"
        );
    }

    #[test]
    fn nested_block_forward_references() {
        k9::snapshot!(
            test_body(
                "
foo = x
  x = y
    y = bar
bar = 10
"
            ),
            "
foo (n) = (let ((x (let ((y bar)) y))) x)
bar (n) = 10
"
        );

        k9::snapshot!(
            test_body(
                "
foo = x
  x = y
    y = bar
      bar = 10
bar = 20
"
            ),
            "
foo (n) = (let ((x (let ((y (let ((bar 10)) bar))) y))) x)
bar_1 (n) = 20
"
        );
    }

    #[test]
    fn block_without_value() {
        k9::snapshot!(
            test_body(
                "
foo =
  x = 10
"
            ),
            "foo failed: BlockWithoutResult"
        );
    }

    #[test]
    fn subassignment_failure() {
        k9::snapshot!(
            test_body(
                "
foo =
  x = [+]
  10
"
            ),
            "foo failed: SubAssignmentFailed"
        );
    }
}
