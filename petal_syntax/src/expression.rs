use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Compound(HashMap<RichIdentifier, Expression>, Box<Expression>),
    Implicit(Builtin),
    Identifier(RichIdentifier),
    NumericLiteral(String),
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
            Compound(assignments, expr) => {
                write!(f, "(let (")?;
                let mut keys: Vec<&RichIdentifier> = assignments.keys().collect();
                keys.sort_by_key(|rich_id| rich_id.id);
                for (i, id) in keys.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "({} {})", id, assignments.get(id).unwrap())?;
                }
                write!(f, ") {})", expr)
            }
            Identifier(rich_id) => write!(f, "{}", rich_id),
            NumericLiteral(num) => write!(f, "{}", num),
            Implicit(builtin) => write!(f, "<{}>", builtin),
            Parens(expr) => write!(f, "{}", expr),
            Brackets(exprs) => {
                write!(f, "[")?;
                for (i, expr) in exprs.iter().rev().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", expr)?;
                }
                write!(f, "]")
            }
            Tuple(exprs) => {
                if exprs.is_empty() {
                    write!(f, "<unit>")
                } else {
                    write!(f, "(<tuple>")?;
                    for expr in exprs.iter().rev() {
                        write!(f, " {}", expr)?;
                    }
                    write!(f, ")")
                }
            }
            UnaryApplication(func, expr) => {
                write!(f, "({} {})", func, expr)
            }
            BinaryApplication(func, lhs, rhs) => {
                write!(f, "({} {} {})", func, lhs, rhs)
            }
        }
    }
}
