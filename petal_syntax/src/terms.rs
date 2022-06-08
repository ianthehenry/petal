use core::fmt;
use std::hash::{Hash, Hasher};

// We go through multiple parsing passes, changing the type of terms each time.
//
// First we create SemiSoupyTerms, which do not know how to split sequences of
// punctuation characters into individual identifier tokens, and which preserve
// spaces. Spaces are necessary to disambiguate unary negation from subtraction.
//
// Then we get rid of semicolons, converting them into parens or brackets.
//
// Next we do a treewalk that uses currently defined operators to split
// punctuation soup into individual operator tokens.
//
// Then we do a pass that handles unary negation and implicit multiplication,
// eliminating spaces and rewriting the Operator type to a normal identifier.
//
// Finally we run a complicated part-of-speech parser that produces the final
// parse output.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum SemiSoupyTerm {
    Identifier(String),
    PunctuationSoup(String),
    NumericLiteral(String),
    Parens(Vec<SemiSoupyTerm>),
    Brackets(Vec<SemiSoupyTerm>),
    Semicolons(usize),
    Space,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum SoupyTerm {
    Identifier(String),
    PunctuationSoup(String),
    NumericLiteral(String),
    Parens(Vec<SoupyTerm>),
    Brackets(Vec<SoupyTerm>),
    Space,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum SouplessTerm {
    Identifier(String),
    Operator(String),
    MinusOperator,
    NumericLiteral(String),
    Parens(Vec<SouplessTerm>),
    Brackets(Vec<SouplessTerm>),
    Space,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum SpacelessTerm {
    Identifier(String),
    NumericLiteral(String),
    Coefficient(String),
    Parens(Vec<SpacelessTerm>),
    Brackets(Vec<SpacelessTerm>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Implicit(Builtin),
    Atom(Atom),
    Parens(Box<Term>),
    // TODO: currently tuples and brackets store their elements in reverse
    // order, which is a dumb performance hack.
    Tuple(Vec<Term>),
    Brackets(Vec<Term>),
    UnaryApplication(Box<Term>, Box<Term>),
    BinaryApplication(Box<Term>, Box<Term>, Box<Term>),
}

impl Term {
    pub(super) fn unary(f: Term, x: Term) -> Self {
        Term::UnaryApplication(Box::new(f), Box::new(x))
    }

    pub(super) fn binary(f: Term, x: Term, y: Term) -> Self {
        Term::BinaryApplication(Box::new(f), Box::new(x), Box::new(y))
    }

    pub(super) fn num(num: String) -> Self {
        Term::Atom(Atom::NumericLiteral(num))
    }

    pub(super) fn id(id: RichIdentifier) -> Self {
        Term::Atom(Atom::Identifier(id))
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

fn listed<T: fmt::Display>(terms: &[T], f: &mut fmt::Formatter) -> fmt::Result {
    let mut first = true;
    for term in terms {
        if first {
            first = false;
        } else {
            write!(f, " ")?;
        }
        write!(f, "{}", term)?;
    }
    Ok(())
}

impl fmt::Display for SemiSoupyTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SemiSoupyTerm::*;
        match self {
            Parens(terms) => {
                write!(f, "(")?;
                listed(terms, f)?;
                write!(f, ")")
            }
            Brackets(terms) => {
                write!(f, "[")?;
                listed(terms, f)?;
                write!(f, "]")
            }
            Space => write!(f, "␠"),
            Semicolons(count) => write!(f, "{}", ";".repeat(*count)),
            Identifier(s) | PunctuationSoup(s) | NumericLiteral(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for SoupyTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SoupyTerm::*;
        match self {
            Parens(terms) => {
                write!(f, "(")?;
                listed(terms, f)?;
                write!(f, ")")
            }
            Brackets(terms) => {
                write!(f, "[")?;
                listed(terms, f)?;
                write!(f, "]")
            }
            Space => write!(f, "␠"),
            Identifier(s) | PunctuationSoup(s) | NumericLiteral(s) => write!(f, "{}", s),
        }
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

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
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
