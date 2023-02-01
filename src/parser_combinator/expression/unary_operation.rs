use nom::{branch::alt, bytes::complete::tag, combinator::map, sequence::terminated, IResult};

use crate::{parser_combinator::single_space_or_comment, visit_rhs::RhsVisitable};

use super::{space_or_comment0, Expression, FromSpan, Span, SpanParseError};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnaryOperation<'a> {
    operator: UnaryOperator<'a>,
    expression: Box<Expression<'a>>,
}

impl<'a> RhsVisitable<Expression<'a>> for UnaryOperation<'a> {
    fn visit_rhs_mod_travel_up<F, R>(&mut self, function: &F) -> R
    where
        F: Fn(&mut Expression<'a>, Option<R>) -> R,
    {
        // no way to call function here, but upper expression will do that
        self.expression.visit_rhs_mod_travel_up(function)
    }
}

impl<'a> UnaryOperation<'a> {
    pub fn operator(&self) -> &UnaryOperator<'a> {
        &self.operator
    }

    pub fn expression(&self) -> &Expression {
        self.expression.as_ref()
    }

    pub fn expression_mut(&mut self) -> &mut Box<Expression<'a>> {
        &mut self.expression
    }
}

impl<'a> FromSpan<'a> for UnaryOperation<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        let (s, operator) = UnaryOperator::parse_span(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, expression) = Expression::parse_span(s)?;
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
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        alt((
            map(tag("-"), Self::Minus),
            map(terminated(tag("not"), single_space_or_comment), Self::Not),
        ))(s)
    }
}
