use std::ops::RangeFrom;

use nom::{
    character::complete::anychar, error::ParseError, AsChar, IResult, InputIter, InputLength,
    Parser, Slice,
};

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

pub fn matching_char<I, E, F>(pred: F) -> impl FnMut(I) -> IResult<I, char, E>
where
    F: Fn(char) -> bool,
    I: Clone + InputLength + InputIter + Slice<RangeFrom<usize>>,
    E: ParseError<I>,
    <I as InputIter>::Item: AsChar,
{
    move |input: I| -> Result<(I, char), _> {
        let i = input.clone();

        let (input, o) = anychar.parse(input)?;

        if pred(o) {
            Ok((input, o))
        } else {
            Err(nom::Err::Error(E::from_error_kind(
                i,
                nom::error::ErrorKind::Verify,
            )))
        }
    }
}
