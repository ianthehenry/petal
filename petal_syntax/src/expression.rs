use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Implicit(Builtin),
    Atom(Atom),
    Parens(Box<Expression>),
    // TODO: currently tuples and brackets store their elements in reverse
    // order, which is a dumb performance hack.
    Tuple(Vec<Expression>),
    Brackets(Vec<Expression>),
    UnaryApplication(Box<Expression>, Box<Expression>),
    BinaryApplication(Box<Expression>, Box<Expression>, Box<Expression>),
}

impl Expression {
    pub(super) fn unary(f: Expression, x: Expression) -> Self {
        Expression::UnaryApplication(Box::new(f), Box::new(x))
    }

    pub(super) fn binary(f: Expression, x: Expression, y: Expression) -> Self {
        Expression::BinaryApplication(Box::new(f), Box::new(x), Box::new(y))
    }

    pub(super) fn num(num: String) -> Self {
        Expression::Atom(Atom::NumericLiteral(num))
    }

    pub(super) fn id(id: RichIdentifier) -> Self {
        Expression::Atom(Atom::Identifier(id))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Builtin {
    Scale,
    PartialApplicationLeft,
    PartialApplicationRight,
    Compose,
    ComposeLeft,
    ComposeRight,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Atom {
    NumericLiteral(String),
    Identifier(RichIdentifier),
}

pub type Identifier = u64;

#[derive(Eq, Debug, Clone)]
pub struct RichIdentifier {
    pub id: Identifier,
    pub name: String,
}

impl RichIdentifier {
    pub(super) fn new(id: Identifier, name: String) -> Self {
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

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::NumericLiteral(num) => write!(f, "{}", num),
            Atom::Identifier(id) => write!(f, "{}", id),
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Builtin::*;
        match self {
            Scale => write!(f, "scale"),
            PartialApplicationLeft => write!(f, "lhs"),
            PartialApplicationRight => write!(f, "rhs"),
            Compose => write!(f, "comp"),
            ComposeLeft => write!(f, "comp-lhs"),
            ComposeRight => write!(f, "comp-rhs"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;
        match self {
            Atom(atom) => write!(f, "{}", atom),
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
