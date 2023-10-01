//! Lookaround helpers for quickly implementing optimizations
use crate::prelude::*;

pub(crate) enum Lookaround<T> {
    Stop(T),
    Abort,
    Continue,
}

pub(crate) fn lookahead<'a, I, F, T>(
    instructions: I,
    from: usize,
    mut callback: F,
) -> Option<(usize, T)>
where
    I: IntoIterator<Item = &'a MindustryOperation>,
    F: FnMut(&'a MindustryOperation) -> Lookaround<T>,
{
    for (index, instruction) in instructions.into_iter().enumerate().skip(from) {
        match callback(instruction) {
            Lookaround::Stop(value) => {
                return Some((index, value));
            }
            Lookaround::Abort => return None,
            Lookaround::Continue => {}
        }
    }

    None
}

pub(crate) fn lookbehind<'a, I, F, T>(
    instructions: I,
    from: usize,
    mut callback: F,
) -> Option<(usize, T)>
where
    I: IntoIterator<Item = &'a MindustryOperation>,
    <I as IntoIterator>::IntoIter: ExactSizeIterator + DoubleEndedIterator,
    F: FnMut(&'a MindustryOperation) -> Lookaround<T>,
{
    for (index, instruction) in instructions.into_iter().enumerate().take(from).rev() {
        match callback(instruction) {
            Lookaround::Stop(value) => {
                return Some((index, value));
            }
            Lookaround::Abort => return None,
            Lookaround::Continue => {}
        }
    }

    None
}
