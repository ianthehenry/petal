use nom_locate::LocatedSpan;
pub(super) type Span<'a> = LocatedSpan<&'a str>;

pub(super) fn new_span(s: &str) -> Span {
    LocatedSpan::new(s)
}
