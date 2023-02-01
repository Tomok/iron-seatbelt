use std::mem;

use nom::{branch::alt, bytes::complete::tag, combinator::map, IResult};

use crate::visit_rhs::RhsVisitable;

use super::{BracketOperation, Expression, FromSpan, IdentPath, IntLiteral, Span, SpanParseError};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct BinaryOperation<'a> {
    /// left hand side of the operation
    lhs: Box<Expression<'a>>,
    operator: BinaryOperator<'a>,
    /// right hand side of the operation
    rhs: Box<Expression<'a>>,
}

impl<'a> RhsVisitable<Expression<'a>> for BinaryOperation<'a> {
    fn visit_rhs_mod_travel_up<F, R>(&mut self, function: &F) -> R
    where
        F: Fn(&mut Expression<'a>, Option<R>) -> R,
    {
        // no way to call function here, but upper expression will do that
        self.rhs.visit_rhs_mod_travel_up(function)
    }
}

impl<'a> BinaryOperation<'a> {
    pub fn new(
        lhs: Box<Expression<'a>>,
        operator: BinaryOperator<'a>,
        rhs: Box<Expression<'a>>,
    ) -> Self {
        Self { lhs, operator, rhs }
    }

    pub fn lhs(&self) -> &Expression {
        self.lhs.as_ref()
    }

    pub fn operator(&self) -> &BinaryOperator<'a> {
        &self.operator
    }

    pub fn rhs(&self) -> &Expression {
        self.rhs.as_ref()
    }

    pub fn lhs_mut(&mut self) -> &mut Box<Expression<'a>> {
        &mut self.lhs
    }

    pub fn operator_mut(&mut self) -> &mut BinaryOperator<'a> {
        &mut self.operator
    }

    pub fn rhs_mut(&mut self) -> &mut Box<Expression<'a>> {
        &mut self.rhs
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinaryOperator<'s> {
    Add(Span<'s>),
    Sub(Span<'s>),
    Mul(Span<'s>),
    Div(Span<'s>),
    Mod(Span<'s>),

    And(Span<'s>),
    Or(Span<'s>),

    /// `=`
    Assign(Span<'s>),

    Eq(Span<'s>),
    /// `!=`
    Neq(Span<'s>),
    Le(Span<'s>),
    Ge(Span<'s>),
    Lt(Span<'s>),
    Gt(Span<'s>),
    //   Comma(Span<'s>),
}

impl<'a> BinaryOperator<'a> {
    pub fn cmp_bind_strength(&self, other: &Self) -> std::cmp::Ordering {
        const fn operator_to_bind_strength(o: &BinaryOperator<'_>) -> i8 {
            match o {
                BinaryOperator::Mul(_) => 4,
                BinaryOperator::Div(_) => 4,
                BinaryOperator::Mod(_) => 4,
                BinaryOperator::Add(_) => 3,
                BinaryOperator::Sub(_) => 3,
                BinaryOperator::And(_) => 2,
                BinaryOperator::Or(_) => 2,
                BinaryOperator::Eq(_) => 1,
                BinaryOperator::Neq(_) => 1,
                BinaryOperator::Le(_) => 1,
                BinaryOperator::Ge(_) => 1,
                BinaryOperator::Lt(_) => 1,
                BinaryOperator::Gt(_) => 1,
                BinaryOperator::Assign(_) => 0,
                //BinaryOperator::Comma(_) => -1,
            }
        }

        operator_to_bind_strength(self).cmp(&operator_to_bind_strength(other))
    }
}

/// Assoiciativity of Binary Operation in case multiple values with the same bind strength are used
pub enum BinaryOperationAssociativity {
    /// `1 / 2 / 3 == (1 / 2) / 3`
    LeftToRight,
    /// ` a = b = 7; assert_eq!(a, 7); assert_eq(b, 7); `
    RightToLeft,
    RequireParentheses,
}

impl BinaryOperationAssociativity {
    /// Returns `true` if the binary operation associativity is [`LeftToRight`].
    ///
    /// [`LeftToRight`]: BinaryOperationAssociativity::LeftToRight
    #[must_use]
    pub fn is_left_to_right(&self) -> bool {
        matches!(self, Self::LeftToRight)
    }

    /// Returns `true` if the binary operation associativity is [`RightToLeft`].
    ///
    /// [`RightToLeft`]: BinaryOperationAssociativity::RightToLeft
    #[must_use]
    pub fn is_right_to_left(&self) -> bool {
        matches!(self, Self::RightToLeft)
    }

    /// Returns `true` if the binary operation associativity is [`RequireParentheses`].
    ///
    /// [`RequireParentheses`]: BinaryOperationAssociativity::RequireParentheses
    #[must_use]
    pub fn is_require_parentheses(&self) -> bool {
        matches!(self, Self::RequireParentheses)
    }
}

impl<'s> BinaryOperator<'s> {
    pub fn parse_span<E: SpanParseError<'s>>(s: Span<'s>) -> IResult<Span, Self, E> {
        alt((
            map(tag("+"), Self::Add),
            map(tag("-"), Self::Sub),
            map(tag("*"), Self::Mul),
            map(tag("/"), Self::Div),
            map(tag("mod"), Self::Mod),
            map(tag("and"), Self::And),
            map(tag("or"), Self::Or),
            map(tag("=="), Self::Eq),
            map(tag("!="), Self::Neq),
            map(tag("<="), Self::Le),
            map(tag(">="), Self::Ge),
            map(tag("<"), Self::Lt),
            map(tag(">"), Self::Gt),
            map(tag("="), Self::Assign),
        ))(s)
    }

    #[must_use]
    pub fn assoiciativity(&self) -> BinaryOperationAssociativity {
        match self {
            BinaryOperator::Add(_)
            | BinaryOperator::Sub(_)
            | BinaryOperator::Mul(_)
            | BinaryOperator::Div(_)
            | BinaryOperator::Mod(_)
            | BinaryOperator::And(_)
            | BinaryOperator::Or(_)
            | BinaryOperator::Eq(_)
            | BinaryOperator::Neq(_)
            | BinaryOperator::Le(_)
            | BinaryOperator::Ge(_)
            | BinaryOperator::Lt(_)
            | BinaryOperator::Gt(_) => BinaryOperationAssociativity::LeftToRight,
            BinaryOperator::Assign(_) => BinaryOperationAssociativity::RightToLeft,
        }
    }

    #[must_use]
    pub fn span<'a>(&'a self) -> &'a Span<'s> {
        match self {
            BinaryOperator::Add(s)
            | BinaryOperator::Sub(s)
            | BinaryOperator::Mul(s)
            | BinaryOperator::Div(s)
            | BinaryOperator::Mod(s)
            | BinaryOperator::And(s)
            | BinaryOperator::Or(s)
            | BinaryOperator::Eq(s)
            | BinaryOperator::Neq(s)
            | BinaryOperator::Le(s)
            | BinaryOperator::Ge(s)
            | BinaryOperator::Lt(s)
            | BinaryOperator::Gt(s)
            | BinaryOperator::Assign(s) => s,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BinarySequenceOperant<'a> {
    BracketOperation(BracketOperation<'a>),
    IntLiteral(IntLiteral<'a>),
    IdentPath(IdentPath<'a>),
}

impl<'a> FromSpan<'a> for BinarySequenceOperant<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        alt((
            map(BracketOperation::parse_span, Self::BracketOperation),
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}

impl<'a> From<BinarySequenceOperant<'a>> for Expression<'a> {
    fn from(value: BinarySequenceOperant<'a>) -> Self {
        match value {
            BinarySequenceOperant::BracketOperation(b) => b.into(),
            BinarySequenceOperant::IntLiteral(i) => i.into(),
            BinarySequenceOperant::IdentPath(i) => i.into(),
        }
    }
}

#[cfg(test)]
mod test {
    /* TODO
    use super::*;
    use nom::{combinator::all_consuming, error::VerboseError};
    use nom_locate::LocatedSpan;
    use rstest::rstest;
    #[rstest]
    #[case::addition("+")]
    #[case::substraction("-")]
    fn test_add_sub_simple(#[case] operator: &str) {
        let input = format!("a {} b", operator);
        let span = LocatedSpan::new(input.as_str());
        let (_, res) =
            all_consuming::<_, _, VerboseError<_>, _>(BinaryOperation::parse_span)(span).unwrap();
        assert_eq!(&operator, res.operator().span().fragment());
        assert_eq!("a", &res.lhs().as_ident_path().unwrap().to_string());
        assert_eq!("b", &res.rhs().as_ident_path().unwrap().to_string());
    }

    #[test]
    fn test_complex_arithmetic() {
        let input = "a + b * c - c";
        let span = LocatedSpan::new(input);
        let (_, res) =
            all_consuming::<_, _, VerboseError<_>, _>(BinaryOperation::parse_span)(span).unwrap();
        assert_eq!(&"-", res.operator().span().fragment());
        let addition = res.lhs().as_binary_operation().unwrap();
        assert_eq!(&"+", addition.operator().span().fragment());
        let multiplication = addition.rhs().as_binary_operation().unwrap();
        assert_eq!(&"*", multiplication.operator().span().fragment());
    }*/
}
