use std::fmt;
use std::marker::PhantomData;
use std::ops::Range;

use crate::{ParseError, Parser};

pub trait LogosSupport<'source, T, Input, O>
where
    Input: logos::Source<'source>,
    T: logos::Logos + fmt::Display + fmt::Debug,
{
    fn parse_logos(lexer: logos::Lexer<T, Input>) -> Result<O, ParseError<T>>;
}

impl<'source, T: 'source, Input, Output, P> LogosSupport<'source, T, Input, Output> for P
where
    Input: logos::Source<'source> + 'source,
    P: Parser<'source, T, <Input as logos::Source<'source>>::Slice, Output>,
    T: logos::Logos + logos::source::WithSource<Input> + fmt::Display + fmt::Debug,
{
    fn parse_logos(lexer: logos::Lexer<T, Input>) -> Result<Output, ParseError<T>> {
        let mut shim = LogosShim::wrap(lexer);
        P::parse(&mut shim)
    }
}

struct LogosShim<'source, T, Input>
where
    T: logos::Logos,
    Input: logos::Source<'source>,
{
    inner: logos::Lexer<T, Input>,
    #[allow(dead_code)]
    phantom: &'source PhantomData<Input>,
}

impl<'a, T, I> LogosShim<'a, T, I>
where
    T: logos::Logos,
    I: logos::Source<'a>,
{
    fn wrap(inner: logos::Lexer<T, I>) -> Self {
        Self {
            inner,
            phantom: &PhantomData {},
        }
    }
}

impl<'source, T, Input> crate::Lexer<'source, T, Input::Slice> for LogosShim<'source, T, Input>
where
    T: logos::Logos + logos::source::WithSource<Input>,
    Input: logos::Source<'source>,
{
    const ERROR: T = <T as logos::Logos>::ERROR;
    const END: T = <T as logos::Logos>::END;

    fn advance(&mut self) {
        self.inner.advance();
    }

    fn terminal<'t, 'lexer: 't>(&'lexer self) -> &'t T {
        &self.inner.token
    }

    fn range(&self) -> Range<usize> {
        self.inner.range()
    }

    fn slice(&self) -> Input::Slice {
        self.inner.slice()
    }
}
