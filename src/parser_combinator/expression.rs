use nom::{
    branch::alt,
    combinator::map,
    error::{ContextError, ParseError},
    IResult,
};
use nom_locate::LocatedSpan;

use super::{space_or_comment0, CharLiteral, FromSpan, Ident, IdentPath, IntLiteral, Span};

mod function_call;
pub use function_call::{
    FunctionCall, FunctionCallParameter, FunctionCallParameters, FunctionCallee,
};
mod binary_operation;
pub use binary_operation::{BinaryOperant, BinaryOperation, BinaryOperator};

mod unary_operation;
pub use unary_operation::{UnaryOperation, UnaryOperator};

mod bracket_operation;
pub use bracket_operation::BracketOperation;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression<'a> {
    UnaryOperation(UnaryOperation<'a>),
    IntLiteral(IntLiteral<'a>),
    CharLiteral(CharLiteral<'a>),
    /// an Ident, could be a variable_name or a function name
    FunctionCall(FunctionCall<'a>),
    BinaryOperation(BinaryOperation<'a>),
    BracketOperation(BracketOperation<'a>),
    IdentPath(IdentPath<'a>),
}

impl<'a> FromSpan<'a> for Expression<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(UnaryOperation::parse_span, Self::UnaryOperation),
            map(FunctionCall::parse_span, Self::FunctionCall),
            map(BinaryOperation::parse_span, Self::BinaryOperation),
            map(BracketOperation::parse_span, Self::BracketOperation),
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(CharLiteral::parse_span, Self::CharLiteral),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}
