use std::fmt;

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
pub(super) enum Term {
    Identifier(String),
    NumericLiteral(String),
    Coefficient(String),
    Parens(Vec<Term>),
    Brackets(Vec<Term>),
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
