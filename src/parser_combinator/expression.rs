use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{ContextError, ParseError},
    IResult,
};
use nom_locate::LocatedSpan;

use super::{space_or_comment0, CharLiteral, FromSpan, IdentPath, IntLiteral, Span};

mod function_call;
use function_call::FunctionCall;
mod binary_operation;
pub use binary_operation::{BinaryOperant, BinaryOperation, BinaryOperator};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression<'a> {
    IntLiteral(IntLiteral<'a>),
    CharLiteral(CharLiteral<'a>),
    /// an Ident, could be a variable_name or a function name
    FunctionCall(FunctionCall<'a>),
    BinaryOperation(BinaryOperation<'a>),
    IdentPath(IdentPath<'a>),
}

impl<'a> FromSpan<'a> for Expression<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(CharLiteral::parse_span, Self::CharLiteral),
            map(FunctionCall::parse_span, Self::FunctionCall),
            map(BinaryOperation::parse_span, Self::BinaryOperation),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}
