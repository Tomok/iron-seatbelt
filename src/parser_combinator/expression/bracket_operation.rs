use nom::bytes::complete::tag;

use crate::parser_combinator::FromSpan;

use super::{space_or_comment0, Expression, Span};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct BracketOperation<'a> {
    left_brace: Span<'a>,
    operations: Box<Expression<'a>>,
    right_brace: Span<'a>,
}

impl<'a> FromSpan<'a> for BracketOperation<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, left_brace) = tag("(")(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, operations) = Expression::parse_span(s)?;
        let operations = Box::new(operations);
        let (s, _) = space_or_comment0(s)?;
        let (s, right_brace) = tag(")")(s)?;
        Ok((
            s,
            Self {
                left_brace,
                operations,
                right_brace,
            },
        ))
    }
}
