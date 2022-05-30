use crate::span::Span;

#[derive(Debug, Clone)]
pub(super) struct Location {
    offset: usize,
    line: u32,
}

pub(super) fn span_location(span: Span) -> Location {
    Location {
        offset: span.location_offset(),
        line: span.location_line(),
    }
}
