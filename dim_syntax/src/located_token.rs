use crate::location::*;
use crate::span::*;
use crate::token::*;
#[derive(Debug, Clone)]
pub(super) struct LocatedToken {
    pub(super) location: Location,
    pub(super) token: Token,
}

impl LocatedToken {
    pub(super) fn new(location: Location, token: Token) -> Self {
        LocatedToken { location, token }
    }
    pub(super) fn of_span(span: Span, token: Token) -> Self {
        LocatedToken {
            location: span_location(span),
            token,
        }
    }

    pub(super) fn build<F: Fn(&str) -> Token>(f: F) -> impl Fn(Span) -> LocatedToken {
        move |span: Span| LocatedToken {
            location: span_location(span),
            token: f(span.fragment()),
        }
    }

    pub(super) fn build_string<F: Fn(String) -> Token>(f: F) -> impl Fn(Span) -> LocatedToken {
        move |span: Span| LocatedToken {
            location: span_location(span),
            token: f(span.fragment().to_string()),
        }
    }

    pub(super) fn build_const(token: Token) -> impl Fn(Span) -> LocatedToken {
        move |span: Span| LocatedToken {
            location: span_location(span),
            token: token.clone(),
        }
    }
}
