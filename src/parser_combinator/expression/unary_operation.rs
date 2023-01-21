use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{ContextError, ParseError},
    sequence::terminated,
    IResult,
};
use nom_locate::LocatedSpan;

use crate::parser_combinator::single_space_or_comment;

use super::{space_or_comment0, Expression, FromSpan, Span};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnaryOperation<'a> {
    operator: UnaryOperator<'a>,
    expression: Box<Expression<'a>>,
}
impl<'a> FromSpan<'a> for UnaryOperation<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, operator) = UnaryOperator::parse_span(s)?;
        let (s, _) = space_or_comment0(s)?;
        dbg!((&operator, &s));
        let (s, expression) = Expression::parse_span(s)?;
        dbg!(&s);
        let expression = Box::new(expression);
        Ok((
            s,
            Self {
                operator,
                expression,
            },
        ))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnaryOperator<'a> {
    Not(Span<'a>),
    Minus(Span<'a>),
    // todo: @
}

impl<'a> FromSpan<'a> for UnaryOperator<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(tag("-"), Self::Minus),
            map(terminated(tag("not"), single_space_or_comment), Self::Not),
        ))(s)
    }
}
