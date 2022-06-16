use crate::span::Span;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(super) struct Location {
    offset: usize,
    line: u32,
}

impl Location {
    pub(super) fn of_span(span: &Span) -> Self {
        Location {
            offset: span.location_offset(),
            line: span.location_line(),
        }
    }
}
