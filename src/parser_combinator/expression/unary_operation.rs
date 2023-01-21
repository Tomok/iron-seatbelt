use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    combinator::{map, peek},
    error::{ContextError, ParseError},
    sequence::terminated,
    IResult,
};
use nom_locate::LocatedSpan;

use crate::parser_combinator::{single_space_or_comment, IdentPath};

use super::{space_or_comment0, BracketOperation, Expression, FromSpan, FunctionCall, Span};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnaryOperationLeft<'a> {
    operator: UnaryOperatorLeft<'a>,
    expression: Box<Expression<'a>>,
}

impl<'a> FromSpan<'a> for UnaryOperationLeft<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, operator) = UnaryOperatorLeft::parse_span(s)?;
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
pub enum UnaryOperatorLeft<'a> {
    Not(Span<'a>),
    Minus(Span<'a>),
}

impl<'a> FromSpan<'a> for UnaryOperatorLeft<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(tag("-"), Self::Minus),
            map(terminated(tag("not"), single_space_or_comment), Self::Not),
        ))(s)
    }
}

mod unary_operator_right_intermediate {
    use nom::{multi::many1, sequence::preceded};

    ///! Intermediate structs to parse the right aligned unary Operators
    use crate::parser_combinator::{
        expression::{BracketOperation, FunctionCall},
        IdentPath,
    };

    use super::*;

    pub struct UnaryOperatorRightSequence<'a> {
        operant: UnaryOperatorRightSequenceOperant<'a>,
        operators: Vec<UnaryOperatorRight<'a>>,
    }

    impl<'a> From<UnaryOperatorRightSequence<'a>> for UnaryOperationRight<'a> {
        fn from(value: UnaryOperatorRightSequence<'a>) -> Self {
            assert!(!value.operators.is_empty());
            let inner_operant = value.operant.into();
            let mut result = Self {
                operant: inner_operant,
                operator: value.operators[0].clone(),
            };
            for operator in value.operators.iter().skip(1) {
                result = Self {
                    operator: operator.clone(),
                    operant: UnaryOperantRight::UnaryOperationRight(Box::new(result)),
                };
            }
            result
        }
    }
    impl<'a> FromSpan<'a> for UnaryOperatorRightSequence<'a> {
        fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
            s: Span<'a>,
        ) -> IResult<Span, Self, E> {
            let (s, operant) = UnaryOperatorRightSequenceOperant::parse_span(s)?;
            let (s, operators) =
                many1(preceded(space_or_comment0, UnaryOperatorRight::parse_span))(s)?;
            dbg!((&operant, &operators));
            Ok((s, Self { operant, operators }))
        }
    }

    #[derive(Debug)]
    enum UnaryOperatorRightSequenceOperant<'a> {
        BracketOperation(BracketOperation<'a>),
        FunctionCall(FunctionCall<'a>),
        IdentPath(IdentPath<'a>),
    }

    impl<'a> From<UnaryOperatorRightSequenceOperant<'a>> for UnaryOperantRight<'a> {
        fn from(value: UnaryOperatorRightSequenceOperant<'a>) -> Self {
            match value {
                UnaryOperatorRightSequenceOperant::BracketOperation(b) => Self::BracketOperation(b),
                UnaryOperatorRightSequenceOperant::FunctionCall(f) => Self::FunctionCall(f),
                UnaryOperatorRightSequenceOperant::IdentPath(i) => Self::IdentPath(i),
            }
        }
    }

    impl<'a> FromSpan<'a> for UnaryOperatorRightSequenceOperant<'a> {
        fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
            s: Span<'a>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(BracketOperation::parse_span, Self::BracketOperation),
                map(FunctionCall::parse_span, Self::FunctionCall),
                map(IdentPath::parse_span, Self::IdentPath),
            ))(s)
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnaryOperationRight<'a> {
    operator: UnaryOperatorRight<'a>,
    operant: UnaryOperantRight<'a>,
}

impl<'a> FromSpan<'a> for UnaryOperationRight<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, sequence) =
            unary_operator_right_intermediate::UnaryOperatorRightSequence::parse_span(s)?;
        let operation = sequence.into();
        Ok((s, operation))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnaryOperantRight<'a> {
    BracketOperation(BracketOperation<'a>),
    FunctionCall(FunctionCall<'a>),
    IdentPath(IdentPath<'a>),
    UnaryOperationRight(Box<UnaryOperationRight<'a>>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnaryOperatorRight<'a> {
    /// the `!` operator used to get to where a pointer points too
    Deref(Span<'a>),
    /// the `@` operator used to get the address of a variable
    Addr(Span<'a>),
}

impl<'a> FromSpan<'a> for UnaryOperatorRight<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            //prevent this from detecting
            //the ! in `!=` as deref by ensuring there is no `=`
            map(terminated(tag("!"), peek(is_not("="))), Self::Deref),
            map(tag("@"), Self::Addr),
        ))(s)
    }
}
