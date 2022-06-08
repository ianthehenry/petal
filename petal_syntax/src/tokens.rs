use crate::located_token::*;
use nom::{IResult, InputLength, InputTake};
use std::num::NonZeroUsize;

#[derive(Debug, Clone)]
pub(super) struct Tokens<'a>(pub(super) &'a [LocatedToken]);

impl<'a> Tokens<'a> {
    pub(super) fn new(x: &'a [LocatedToken]) -> Self {
        Tokens(x)
    }

    // TODO: I would like to use this function and make the constructor private,
    // but for some reason it doesn't work. I think some kind of lifetime
    // trouble.
    pub(super) fn first(&'a self) -> &'a LocatedToken {
        &self.0[0]
    }

    pub(super) fn rest(&self) -> Self {
        Tokens(&self.0[1..])
    }

    pub(super) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'a> nom::InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Tokens(&self.0[0..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (Tokens(suffix), Tokens(prefix))
    }
}

impl<'a> nom::InputTakeAtPosition for Tokens<'a> {
    type Item = &'a LocatedToken;

    fn split_at_position<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(i) => Ok(self.take_split(i)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position1(predicate, e) {
            Err(nom::Err::Incomplete(_)) => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
            res => res,
        }
    }
}

impl<'a> nom::InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl nom::InputLength for LocatedToken {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> nom::InputIter for Tokens<'a> {
    type Item = &'a LocatedToken;
    type Iter = std::iter::Enumerate<Self::IterElem>;
    type IterElem = std::slice::Iter<'a, LocatedToken>;

    fn iter_indices(&self) -> Self::Iter {
        self.0.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        let len = self.0.len();
        if len >= count {
            Ok(count)
        } else {
            let needed = unsafe { NonZeroUsize::new_unchecked(count - len) };
            Err(nom::Needed::Size(needed))
        }
    }
}
