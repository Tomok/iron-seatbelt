use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{ContextError, ParseError},
    IResult,
};
use nom_locate::LocatedSpan;

use super::{
    space_or_comment0, BracketOperation, FromSpan, FunctionCall, IdentPath, IntLiteral, Span,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct BinaryOperation<'a> {
    /// left hand side of the operation
    lhs: Box<BinaryOperant<'a>>,
    operator: BinaryOperator<'a>,
    /// right hand side of the operation
    rhs: Box<BinaryOperant<'a>>,
}

impl<'a> FromSpan<'a> for BinaryOperation<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, seq) = BinaryOperationSequence::parse_span(s)?;
        let res = Self::try_from(seq).unwrap(); //TODO: handle errors!
        Ok((s, res))
    }
}

impl<'a> TryFrom<BinaryOperationSequence<'a>> for BinaryOperation<'a> {
    type Error = ();

    fn try_from(value: BinaryOperationSequence<'a>) -> Result<Self, Self::Error> {
        Self::from_operators_and_operants(&value.operators, &value.operants)
    }
}

impl<'a> BinaryOperation<'a> {
    fn from_operators_and_operants(
        operators: &[BinaryOperator<'a>],
        operants: &[BinarySequenceOperant<'a>],
    ) -> Result<Self, ()> {
        assert!(!operators.is_empty());
        let mut weakest_operator_idx = 0;
        let mut order_of_operators_determined = true;
        for (idx, operator) in operators.iter().enumerate().skip(1) {
            match (
                operator.cmp_bind_strength(&operators[weakest_operator_idx]),
                operator.assoiciativity(),
            ) {
                (std::cmp::Ordering::Equal, BinaryOperationAssociativity::LeftToRight)
                | (std::cmp::Ordering::Less, _) => {
                    weakest_operator_idx = idx;
                    order_of_operators_determined = true
                }
                (std::cmp::Ordering::Equal, BinaryOperationAssociativity::RightToLeft)
                | (std::cmp::Ordering::Greater, _) => {}
                (
                    std::cmp::Ordering::Equal,
                    BinaryOperationAssociativity::RequireParentheses,
                ) => order_of_operators_determined = false,
            }
        }
        if !order_of_operators_determined {
            todo!(
                "raise error here = Parentheses necessary to determine order of operations..."
            )
        }
        let (left_operators, mid_right_operators) = operators.split_at(weakest_operator_idx);
        let (left_operants, right_operants) = operants.split_at(weakest_operator_idx + 1);
        dbg!((&left_operators, &mid_right_operators));
        dbg!((&left_operants, &right_operants));
        let rhs = if mid_right_operators.is_empty() {
            assert_eq!(1, right_operants.len());
            Box::new(right_operants[0].clone().into())
        } else {
            let (_, right_operators) = mid_right_operators.split_at(1);
            Box::new(BinaryOperant::from_operators_and_operants(
                right_operators,
                right_operants,
            )?)
        };

        let lhs = if left_operators.is_empty() {
            assert_eq!(1, left_operants.len());
            Box::new(left_operants[0].clone().into())
        } else {
            Box::new(BinaryOperant::from_operators_and_operants(
                left_operators,
                left_operants,
            )?)
        };

        let operator = operators[weakest_operator_idx].clone();
        Ok(Self { lhs, operator, rhs })
    }

    pub fn lhs(&self) -> &BinaryOperant {
        self.lhs.as_ref()
    }

    pub fn operator(&self) -> &BinaryOperator<'a> {
        &self.operator
    }

    pub fn rhs(&self) -> &BinaryOperant {
        self.rhs.as_ref()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinaryOperant<'a> {
    BracketOperation(BracketOperation<'a>),
    FunctionCall(FunctionCall<'a>),
    IntLiteral(IntLiteral<'a>),
    IdentPath(IdentPath<'a>),
    BinaryOperation(BinaryOperation<'a>),
}

impl<'a> BinaryOperant<'a> {
    fn from_operators_and_operants(
        operators: &[BinaryOperator<'a>],
        operants: &[BinarySequenceOperant<'a>],
    ) -> Result<Self, ()> {
        Ok(if operators.is_empty() {
            assert_eq!(operants.len(), 1);
            Self::from(operants[0].clone())
        } else {
            Self::BinaryOperation(BinaryOperation::from_operators_and_operants(
                operators, operants,
            )?)
        })
    }

    /// Returns `true` if the binary operant is [`FunctionCall`].
    ///
    /// [`FunctionCall`]: BinaryOperant::FunctionCall
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

    /// Returns `true` if the binary operant is [`IntLiteral`].
    ///
    /// [`IntLiteral`]: BinaryOperant::IntLiteral
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

    /// Returns `true` if the binary operant is [`IdentPath`].
    ///
    /// [`IdentPath`]: BinaryOperant::IdentPath
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

    /// Returns `true` if the binary operant is [`BinaryOperation`].
    ///
    /// [`BinaryOperation`]: BinaryOperant::BinaryOperation
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

    /// Returns `true` if the binary operant is [`Bracket`].
    ///
    /// [`Bracket`]: BinaryOperant::Bracket
    #[must_use]
    pub fn is_bracket(&self) -> bool {
        matches!(self, Self::BracketOperation(..))
    }

    pub fn as_bracket(&self) -> Option<&BracketOperation<'a>> {
        if let Self::BracketOperation(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'a> From<BinarySequenceOperant<'a>> for BinaryOperant<'a> {
    fn from(value: BinarySequenceOperant<'a>) -> Self {
        match value {
            BinarySequenceOperant::BracketOperation(bo) => Self::BracketOperation(bo),
            BinarySequenceOperant::FunctionCall(fc) => Self::FunctionCall(fc),
            BinarySequenceOperant::IntLiteral(il) => Self::IntLiteral(il),
            BinarySequenceOperant::IdentPath(ip) => Self::IdentPath(ip),
        }
    }
}

///intermediate parsed representation of binary expressions
///
/// read order: operants[0] operator[0] operants[1] ... operator[n] operants[n+1]
#[derive(Debug, Clone)]
struct BinaryOperationSequence<'a> {
    operants: Vec<BinarySequenceOperant<'a>>,
    operators: Vec<BinaryOperator<'a>>,
}

impl<'a> FromSpan<'a> for BinaryOperationSequence<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        const INITIAL_OPERATOR_CAPACITY: usize = 16;
        let mut operators = Vec::with_capacity(INITIAL_OPERATOR_CAPACITY);
        let mut operants = Vec::with_capacity(INITIAL_OPERATOR_CAPACITY + 1);
        let (s, first_operant) = BinarySequenceOperant::parse_span(s)?;
        operants.push(first_operant);
        let (s, _) = space_or_comment0(s)?/*can not fail, so ? should never happen here*/;
        let (mut s, first_operator) = BinaryOperator::parse_span(s)?;
        operators.push(first_operator);
        loop {
            let (new_s, _) = space_or_comment0(s)?/*can not fail, so ? should never happen here*/;
            match BinarySequenceOperant::parse_span(new_s) {
                Err(e) => return Err(e),
                Ok((new_s, operant)) => {
                    let (new_s, _) = space_or_comment0(new_s)?/*can not fail, so ? should never happen here*/;
                    match BinaryOperator::parse_span(new_s) {
                        Err(nom::Err::Failure(f)) => return Err(nom::Err::Failure(f)),
                        Err(nom::Err::Incomplete(needed)) => {
                            return Err(nom::Err::Incomplete(needed))
                        }
                        Err(nom::Err::Error(_)) => {
                            operants.push(operant);
                            //store read location for return
                            s = new_s;
                            break;
                        }
                        Ok((new_s, operator)) => {
                            operators.push(operator);
                            operants.push(operant);
                            //store for next loop
                            s = new_s;
                        }
                    }
                }
            }
        }
        assert!(operants.len() >= 2);
        assert_eq!(operants.len(), operators.len() + 1);
        Ok((
            s,
            Self {
                operants,
                operators,
            },
        ))
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
    fn cmp_bind_strength(&self, other: &Self) -> std::cmp::Ordering {
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

impl<'s> BinaryOperator<'s> {
    pub fn parse_span<
        E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
    >(
        s: Span<'s>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(tag("+"), Self::Add),
            map(tag("-"), Self::Sub),
            map(tag("*"), Self::Mul),
            map(tag("/"), Self::Div),
            map(tag("%"), Self::Mod),
            map(tag("And"), Self::And),
            map(tag("Or"), Self::Or),
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
            | BinaryOperator::Or(_) => BinaryOperationAssociativity::LeftToRight,
            BinaryOperator::Eq(_)
            | BinaryOperator::Neq(_)
            | BinaryOperator::Le(_)
            | BinaryOperator::Ge(_)
            | BinaryOperator::Lt(_)
            | BinaryOperator::Gt(_) => BinaryOperationAssociativity::RequireParentheses,
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

#[derive(Clone, Debug)]
enum BinarySequenceOperant<'a> {
    BracketOperation(BracketOperation<'a>),
    FunctionCall(FunctionCall<'a>),
    IntLiteral(IntLiteral<'a>),
    IdentPath(IdentPath<'a>),
}

impl<'a> FromSpan<'a> for BinarySequenceOperant<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(BracketOperation::parse_span, Self::BracketOperation),
            map(FunctionCall::parse_span, Self::FunctionCall),
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{combinator::all_consuming, error::VerboseError};
    use rstest::rstest;

    #[rstest]
    #[case::addition("+")]
    #[case::substraction("-")]
    fn test_add_sub_simple(#[case] operator: &str) {
        let input = format!("a {} b", operator);
        let span = LocatedSpan::new(input.as_str());
        let (_, res) =
            all_consuming::<_, _, VerboseError<_>, _>(BinaryOperation::parse_span)(span)
                .unwrap();
        assert_eq!(&operator, res.operator().span().fragment());
        assert_eq!("a", &res.lhs().as_ident_path().unwrap().to_string());
        assert_eq!("b", &res.rhs().as_ident_path().unwrap().to_string());
    }

    #[test]
    fn test_complex_arithmetic() {
        let input = "a + b * c - c";
        let span = LocatedSpan::new(input);
        let (_, res) =
            all_consuming::<_, _, VerboseError<_>, _>(BinaryOperation::parse_span)(span)
                .unwrap();
        assert_eq!(&"-", res.operator().span().fragment());
        let addition = res.lhs().as_binary_operation().unwrap();
        assert_eq!(&"+", addition.operator().span().fragment());
        let multiplication = addition.rhs().as_binary_operation().unwrap();
        assert_eq!(&"*", multiplication.operator().span().fragment());
    }
}

