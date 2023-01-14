use nom::{
    branch::alt,
    combinator::map,
    error::{ContextError, ParseError},
    IResult,
};
use nom_locate::LocatedSpan;

use super::{space_or_comment0, CharLiteral, IdentPath, IntLiteral, Span};

mod function_call;
use function_call::FunctionCall;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression<'s> {
    IntLiteral(IntLiteral<'s>),
    CharLiteral(CharLiteral<'s>),
    /// an Ident, could be a variable_name or a function name
    IdentPath(IdentPath<'s>),
    FunctionCall(FunctionCall<'s>),
    //todo operators
}

impl<'s> Expression<'s> {
    pub fn parse_span<E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        s: Span<'s>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(CharLiteral::parse_span, Self::CharLiteral),
            map(FunctionCall::parse_span, Self::FunctionCall),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{combinator::all_consuming, error::VerboseError};

    #[test]
    fn test_operators() {
        let input = "b == c";
        let span = LocatedSpan::new(input);
    }
}
