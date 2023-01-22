use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, peek},
    sequence::{terminated, tuple},
    IResult,
};

use super::{
    space_or_comment0, CharLiteral, FromSpan, IdentPath, IntLiteral, Span, SpanParseError,
};

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
    /// an Ident, could be a variable_name or a function name
    FunctionCall(FunctionCall<'a>),
    BinaryOperation(BinaryOperation<'a>),
    BracketOperation(BracketOperation<'a>),
    Continue(Continue<'a>),
    Break(Break<'a>),
    IntLiteral(IntLiteral<'a>),
    CharLiteral(CharLiteral<'a>),
    IdentPath(IdentPath<'a>),
}

impl<'a> Expression<'a> {
    /// Returns `true` if the expression is [`UnaryOperation`].
    ///
    /// [`UnaryOperation`]: Expression::UnaryOperation
    #[must_use]
    pub fn is_unary_operation(&self) -> bool {
        matches!(self, Self::UnaryOperation(..))
    }

    pub fn as_unary_operation(&self) -> Option<&UnaryOperation<'a>> {
        if let Self::UnaryOperation(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`FunctionCall`].
    ///
    /// [`FunctionCall`]: Expression::FunctionCall
    #[must_use]
    pub fn is_function_call(&self) -> bool {
        matches!(self, Self::FunctionCall(..))
    }

    pub fn as_function_call(&self) -> Option<&FunctionCall<'a>> {
        if let Self::FunctionCall(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`BinaryOperation`].
    ///
    /// [`BinaryOperation`]: Expression::BinaryOperation
    #[must_use]
    pub fn is_binary_operation(&self) -> bool {
        matches!(self, Self::BinaryOperation(..))
    }

    pub fn as_binary_operation(&self) -> Option<&BinaryOperation<'a>> {
        if let Self::BinaryOperation(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`BracketOperation`].
    ///
    /// [`BracketOperation`]: Expression::BracketOperation
    #[must_use]
    pub fn is_bracket_operation(&self) -> bool {
        matches!(self, Self::BracketOperation(..))
    }

    /// Returns `true` if the expression is [`Continue`].
    ///
    /// [`Continue`]: Expression::Continue
    #[must_use]
    pub fn is_continue(&self) -> bool {
        matches!(self, Self::Continue(..))
    }

    pub fn as_continue(&self) -> Option<&Continue<'a>> {
        if let Self::Continue(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`Break`].
    ///
    /// [`Break`]: Expression::Break
    #[must_use]
    pub fn is_break(&self) -> bool {
        matches!(self, Self::Break(..))
    }

    pub fn as_break(&self) -> Option<&Break<'a>> {
        if let Self::Break(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`IntLiteral`].
    ///
    /// [`IntLiteral`]: Expression::IntLiteral
    #[must_use]
    pub fn is_int_literal(&self) -> bool {
        matches!(self, Self::IntLiteral(..))
    }

    pub fn as_int_literal(&self) -> Option<&IntLiteral<'a>> {
        if let Self::IntLiteral(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`CharLiteral`].
    ///
    /// [`CharLiteral`]: Expression::CharLiteral
    #[must_use]
    pub fn is_char_literal(&self) -> bool {
        matches!(self, Self::CharLiteral(..))
    }

    pub fn as_char_literal(&self) -> Option<&CharLiteral<'a>> {
        if let Self::CharLiteral(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expression is [`IdentPath`].
    ///
    /// [`IdentPath`]: Expression::IdentPath
    #[must_use]
    pub fn is_ident_path(&self) -> bool {
        matches!(self, Self::IdentPath(..))
    }

    pub fn as_ident_path(&self) -> Option<&IdentPath<'a>> {
        if let Self::IdentPath(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'a> FromSpan<'a> for Expression<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        alt((
            map(UnaryOperation::parse_span, Self::UnaryOperation),
            map(FunctionCall::parse_span, Self::FunctionCall),
            map(BinaryOperation::parse_span, Self::BinaryOperation),
            map(BracketOperation::parse_span, Self::BracketOperation),
            map(Continue::parse_span, Self::Continue),
            map(Break::parse_span, Self::Break),
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(CharLiteral::parse_span, Self::CharLiteral),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Continue<'a>(Span<'a>);

impl<'a> FromSpan<'a> for Continue<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        map(
            terminated(tag("continue"), peek(tuple((space_or_comment0, tag(";"))))),
            Self,
        )(s)
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Break<'a>(Span<'a>);

impl<'a> FromSpan<'a> for Break<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        map(
            terminated(tag("break"), peek(tuple((space_or_comment0, tag(";"))))),
            Self,
        )(s)
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use nom::error::VerboseError;
    use nom_locate::LocatedSpan;

    #[test]
    fn test_break() {
        let input = "break;";
        let span = LocatedSpan::new(input);
        let (_, res) = Expression::parse_span::<VerboseError<_>>(span).unwrap();
        assert!(
            res.is_break(),
            "should have been a break statement, but was {:#?}",
            res
        );
    }
    #[test]
    fn test_ident_starting_with_break() {
        let input = "break_count;";
        let span = LocatedSpan::new(input);
        let (_, res) = Expression::parse_span::<VerboseError<_>>(span).unwrap();
        assert!(
            !res.is_break(),
            "should have not been a break statement, but was {:#?}",
            res
        );
        let _ident = res.as_ident_path().unwrap();
    }
}
