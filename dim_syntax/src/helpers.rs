use nom::{IResult, Parser};

pub(super) fn replace<I, O1, O2, E, F>(
    mut parser: F,
    value: O2,
) -> impl FnMut(I) -> IResult<I, O2, E>
where
    O2: Clone,
    F: Parser<I, O1, E>,
{
    move |input: I| {
        let (input, _) = parser.parse(input)?;
        Ok((input, value.clone()))
    }
}

pub(super) fn ignore<I, O, E, F>(parser: F) -> impl FnMut(I) -> IResult<I, (), E>
where
    F: Parser<I, O, E>,
{
    replace(parser, ())
}
