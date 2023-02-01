use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    combinator::{map, opt, peek},
    sequence::{preceded, terminated, tuple},
    IResult,
};

use crate::visit_rhs::RhsVisitable;

use self::binary_operation::BinarySequenceOperant;

use super::{
    space_or_comment0, CharLiteral, FromSpan, IdentPath, IntLiteral, Span, SpanParseError,
};

mod function_call;
pub use function_call::{
    FunctionCall, FunctionCallParameter, FunctionCallParameters, FunctionCallee,
};
mod binary_operation;
pub use binary_operation::{BinaryOperation, BinaryOperator};

mod unary_operation;
pub use unary_operation::{UnaryOperation, UnaryOperator};

mod bracket_operation;
pub use bracket_operation::BracketOperation;

trait ExtendAbleExpression<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E>;
}

impl<'a> ExtendAbleExpression<'a> for UnaryOperation<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        match ext {
            ExpressionExtension::FunctionCall(params) => {
                let inner = self.expression_mut();
                *inner = Box::new(FunctionCall::new(inner.clone(), params).into());
                Ok(ExtendSelfOk::Applied(s))
            }
            ExpressionExtension::BinaryOperation(_, _)
            | ExpressionExtension::Deref(_)
            | ExpressionExtension::Address(_) => Ok(ExtendSelfOk::NotApplied((s, ext))),
        }
    }
}

impl<'a> ExtendAbleExpression<'a> for Deref<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        Ok(ExtendSelfOk::NotApplied((s, ext)))
    }
}

impl<'a> ExtendAbleExpression<'a> for Address<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        Ok(ExtendSelfOk::NotApplied((s, ext)))
    }
}

impl<'a> ExtendAbleExpression<'a> for FunctionCall<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        Ok(ExtendSelfOk::NotApplied((s, ext)))
    }
}

impl<'a> ExtendAbleExpression<'a> for BinaryOperation<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        match ext {
            ExpressionExtension::FunctionCall(params) => {
                let rhs = self.rhs_mut();
                let inner = rhs.clone();
                *rhs = Box::new(FunctionCall::new(inner, params).into());
                Ok(ExtendSelfOk::Applied(s))
            }
            ExpressionExtension::BinaryOperation(ref operator, ref operant) => {
                let self_op = self.operator();
                match (
                    self_op.cmp_bind_strength(operator),
                    self_op.assoiciativity(),
                ) {
                    (std::cmp::Ordering::Less, _)
                    | (
                        std::cmp::Ordering::Equal,
                        binary_operation::BinaryOperationAssociativity::RightToLeft,
                    ) => {
                        let rhs = self.rhs_mut();
                        *rhs = Box::new(
                            BinaryOperation::new(
                                rhs.clone(),
                                operator.clone(),
                                Box::new(operant.clone().into()),
                            )
                            .into(),
                        );
                        Ok(ExtendSelfOk::Applied(s))
                    }
                    (std::cmp::Ordering::Greater, _)
                    | (
                        std::cmp::Ordering::Equal,
                        binary_operation::BinaryOperationAssociativity::LeftToRight,
                    ) => Ok(ExtendSelfOk::NotApplied((s, ext))),
                    (
                        std::cmp::Ordering::Equal,
                        binary_operation::BinaryOperationAssociativity::RequireParentheses,
                    ) => todo!(),
                }
            }
            ExpressionExtension::Deref(at) => {
                let rhs = self.rhs_mut();
                *rhs = Box::new(Deref::new(rhs.clone(), at).into());
                Ok(ExtendSelfOk::Applied(s))
            }
            ExpressionExtension::Address(exclamation_mark) => {
                let rhs = self.rhs_mut();
                *rhs = Box::new(Address::new(rhs.clone(), exclamation_mark).into());
                Ok(ExtendSelfOk::Applied(s))
            }
        }
    }
}

impl<'a> ExtendAbleExpression<'a> for BracketOperation<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        Ok(ExtendSelfOk::NotApplied((s, ext)))
    }
}

impl<'a> ExtendAbleExpression<'a> for Continue<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        _s: Span<'a>,
        _ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        todo!("this is not allowed, raise an error")
    }
}

impl<'a> ExtendAbleExpression<'a> for Break<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        _s: Span<'a>,
        _ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        todo!("this is not allowed, raise an error")
    }
}

impl<'a> ExtendAbleExpression<'a> for IntLiteral<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        match ext {
            ExpressionExtension::Deref(_)
            | ExpressionExtension::Address(_)
            | ExpressionExtension::FunctionCall(_) => todo!("this is not allowed, raise an error"),
            ExpressionExtension::BinaryOperation(_, _) => Ok(ExtendSelfOk::NotApplied((s, ext))),
        }
    }
}

impl<'a> ExtendAbleExpression<'a> for CharLiteral<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        match ext {
            ExpressionExtension::Deref(_)
            | ExpressionExtension::Address(_)
            | ExpressionExtension::FunctionCall(_) => todo!("this is not allowed, raise an error"),
            ExpressionExtension::BinaryOperation(_, _) => Ok(ExtendSelfOk::NotApplied((s, ext))),
        }
    }
}

impl<'a> ExtendAbleExpression<'a> for IdentPath<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        Ok(ExtendSelfOk::NotApplied((s, ext)))
    }
}

impl<'a> ExtendAbleExpression<'a> for Expression<'a> {
    fn try_to_extend<E: SpanParseError<'a>>(
        &mut self,
        s: Span<'a>,
        ext: ExpressionExtension<'a>,
    ) -> ExtendSelfResult<'a, E> {
        match self {
            Expression::UnaryOperationLeft(inner) => inner.try_to_extend(s, ext),
            Expression::Deref(inner) => inner.try_to_extend(s, ext),
            Expression::Address(inner) => inner.try_to_extend(s, ext),
            Expression::FunctionCall(inner) => inner.try_to_extend(s, ext),
            Expression::BinaryOperation(inner) => inner.try_to_extend(s, ext),
            Expression::BracketOperation(inner) => inner.try_to_extend(s, ext),
            Expression::Continue(inner) => inner.try_to_extend(s, ext),
            Expression::Break(inner) => inner.try_to_extend(s, ext),
            Expression::IntLiteral(inner) => inner.try_to_extend(s, ext),
            Expression::CharLiteral(inner) => inner.try_to_extend(s, ext),
            Expression::IdentPath(inner) => inner.try_to_extend(s, ext),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Deref<'a> {
    inner: Box<Expression<'a>>,
    at_token: Span<'a>,
}

impl<'a> Deref<'a> {
    pub fn new(inner: Box<Expression<'a>>, at_token: Span<'a>) -> Self {
        Self { inner, at_token }
    }

    pub fn inner(&self) -> &Expression {
        self.inner.as_ref()
    }

    pub fn inner_mut(&mut self) -> &mut Box<Expression<'a>> {
        &mut self.inner
    }

    pub fn at_token(&self) -> Span<'a> {
        self.at_token
    }
} //todo move to submodule
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Address<'a> {
    inner: Box<Expression<'a>>,
    exclamation_mark: Span<'a>,
}

impl<'a> Address<'a> {
    pub fn new(inner: Box<Expression<'a>>, exclamation_mark: Span<'a>) -> Self {
        Self {
            inner,
            exclamation_mark,
        }
    }

    pub fn inner(&self) -> &Expression {
        self.inner.as_ref()
    }

    pub fn inner_mut(&mut self) -> &mut Box<Expression<'a>> {
        &mut self.inner
    }

    pub fn exclamation_mark(&self) -> Span<'a> {
        self.exclamation_mark
    }
} //todo move to submodule

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression<'a> {
    UnaryOperationLeft(UnaryOperation<'a>),
    Deref(Deref<'a>),
    Address(Address<'a>),
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

impl<'a> RhsVisitable<Expression<'a>> for Expression<'a> {
    fn visit_rhs_mod_travel_up<F, R>(&mut self, f: &F) -> R
    where
        F: Fn(&mut Expression<'a>, Option<R>) -> R,
    {
        let child_res = match self {
            Expression::UnaryOperationLeft(op) => Some(op.visit_rhs_mod_travel_up(f)),
            Expression::BinaryOperation(op) => Some(op.visit_rhs_mod_travel_up(f)),
            Expression::FunctionCall(_)
            | Expression::BracketOperation(_)
            | Expression::Continue(_)
            | Expression::Break(_)
            | Expression::IntLiteral(_)
            | Expression::CharLiteral(_)
            | Expression::Deref(_)
            | Expression::Address(_)
            | Expression::IdentPath(_) => None,
        };
        f(self, child_res)
    }
}

impl<'a> From<UnaryOperation<'a>> for Expression<'a> {
    fn from(value: UnaryOperation<'a>) -> Self {
        Expression::UnaryOperationLeft(value)
    }
}

impl<'a> From<Deref<'a>> for Expression<'a> {
    fn from(value: Deref<'a>) -> Self {
        Expression::Deref(value)
    }
}

impl<'a> From<Address<'a>> for Expression<'a> {
    fn from(value: Address<'a>) -> Self {
        Expression::Address(value)
    }
}

impl<'a> From<FunctionCall<'a>> for Expression<'a> {
    fn from(value: FunctionCall<'a>) -> Self {
        Expression::FunctionCall(value)
    }
}

impl<'a> From<BinaryOperation<'a>> for Expression<'a> {
    fn from(value: BinaryOperation<'a>) -> Self {
        Expression::BinaryOperation(value)
    }
}

impl<'a> From<BracketOperation<'a>> for Expression<'a> {
    fn from(value: BracketOperation<'a>) -> Self {
        Expression::BracketOperation(value)
    }
}

impl<'a> From<Continue<'a>> for Expression<'a> {
    fn from(value: Continue<'a>) -> Self {
        Expression::Continue(value)
    }
}

impl<'a> From<Break<'a>> for Expression<'a> {
    fn from(value: Break<'a>) -> Self {
        Expression::Break(value)
    }
}

impl<'a> From<IntLiteral<'a>> for Expression<'a> {
    fn from(value: IntLiteral<'a>) -> Self {
        Expression::IntLiteral(value)
    }
}

impl<'a> From<CharLiteral<'a>> for Expression<'a> {
    fn from(value: CharLiteral<'a>) -> Self {
        Expression::CharLiteral(value)
    }
}

impl<'a> From<IdentPath<'a>> for Expression<'a> {
    fn from(value: IdentPath<'a>) -> Self {
        Expression::IdentPath(value)
    }
}

/// any text that could be behind an expression to become a different expression
#[derive(PartialEq, Eq, Debug, Clone)]
enum ExpressionExtension<'a> {
    FunctionCall(FunctionCallParameters<'a>),
    BinaryOperation(BinaryOperator<'a>, BinarySequenceOperant<'a>),
    Deref(Span<'a>),
    Address(Span<'a>),
}

/*impl<'a> ExpressionExtension<'a> {
    fn try_extend(&self, Expression::IdentPath(i): &Expression<'a>) -> Result<Expression<'a>, ()> {
        use ExpressionExtension::*;
        match self {
            FunctionCall(parameters) => todo!(),
            _ => todo!(),
        }
    }
}*/

impl<'a> FromSpan<'a> for ExpressionExtension<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> IResult<Span, Self, E> {
        alt((
            map(FunctionCallParameters::parse_span, Self::FunctionCall),
            map(
                tuple((
                    BinaryOperator::parse_span,
                    BinarySequenceOperant::parse_span,
                )),
                |(a, b)| Self::BinaryOperation(a, b),
            ),
            map(terminated(tag("!"), peek(is_not("="))), Self::Deref),
            map(tag("@"), Self::Address),
        ))(s)
    }
}

#[derive(Debug)]
enum ExtendSelfOk<'a> {
    /// extension was parsed and applied, nothing more to do
    Applied(Span<'a>),
    /// extension was parsed, but could not be applied on current level,
    /// e.g. current level is an [Expression::IdentPath] and the extension is a
    /// [ExpressionExtension::BinaryOperation] - in this case the function call should be applied
    /// on a higher level to adhere to precedence
    NotApplied((Span<'a>, ExpressionExtension<'a>)),
    /// [parse_extension] returned that no tokens for an extension were found
    NothingToApply(Span<'a>),
}
type ExtendSelfResult<'a, E: SpanParseError<'a>> = Result<ExtendSelfOk<'a>, nom::Err<E>>;

impl<'a> Expression<'a> {
    fn extend<E: SpanParseError<'a>>(
        &mut self,
        s: &Span<'a>,
        child_result: Option<ExtendSelfResult<'a, E>>,
    ) -> ExtendSelfResult<'a, E> {
        use ExtendSelfOk::*;
        match child_result {
            None => {
                let (s, parse_res) = self.parse_extension(*s)?;
                if let Some(parsed) = parse_res {
                    self.try_to_extend(s, parsed)
                } else {
                    Ok(NothingToApply(s))
                }
            }
            Some(Ok(NotApplied((s, to_be_applied)))) => self.try_to_extend(s, to_be_applied),
            // just raise all other results, done manually here to ensure no case was missed
            Some(Ok(Applied(s))) => Ok(Applied(s)),
            Some(Ok(NothingToApply(s))) => Ok(NothingToApply(s)),
            Some(Err(e)) => Err(e),
        }
    }

    /// Returns `true` it the expression could be extended with additional values
    fn extendable(&self) -> bool {
        use Expression::*;
        match self {
            UnaryOperationLeft(_)
            | Deref(_)
            | Address(_)
            | FunctionCall(_)
            | BinaryOperation(_)
            | BracketOperation(_)
            | IntLiteral(_)
            | CharLiteral(_)
            | IdentPath(_) => true,
            Continue(_) | Break(_) => false,
        }
    }

    fn parse_extension<'b, E: SpanParseError<'a>>(
        &'b self,
        s: Span<'a>,
    ) -> IResult<Span<'a>, Option<ExpressionExtension<'a>>, E> {
        let (s, _) = space_or_comment0(s)?;
        match self {
/*            Expression::UnaryOperationLeft(op) => op.expression().parse_extension(s),
            Expression::BinaryOperation(op) => op.rhs().parse_extension(s),*/
            Expression::UnaryOperationLeft(_)
            | Expression::BinaryOperation(_) => panic!("This method should not be called on elements that have a rightmost inner Expression but was called on {:#?}", self),
            Expression::Deref(_)
            | Expression::Address(_)
            | Expression::FunctionCall(_)
            | Expression::BracketOperation(_)
            | Expression::IdentPath(_) => opt(alt((
                map(
                    FunctionCallParameters::parse_span,
                    ExpressionExtension::FunctionCall,
                ),
                map(
                    tuple((
                        BinaryOperator::parse_span,
                        preceded(space_or_comment0, BinarySequenceOperant::parse_span),
                    )),
                    |(a, b)| ExpressionExtension::BinaryOperation(a, b),
                ),
                map(
                    terminated(tag("!"), peek(is_not("="))),
                    ExpressionExtension::Deref,
                ),
                map(tag("@"), ExpressionExtension::Address),
            )))(s),
            Expression::Continue(_) | Expression::Break(_) => Ok((s, None)),
            Expression::IntLiteral(_) | Expression::CharLiteral(_) => opt(alt((
                map(
                    tuple((
                        BinaryOperator::parse_span,
                        preceded(space_or_comment0, BinarySequenceOperant::parse_span),
                    )),
                    |(a, b)| ExpressionExtension::BinaryOperation(a, b),
                ),
                map(tag("@"), ExpressionExtension::Address), //todo: is address of literal allowed?
            )))(s),
        }
    }

    /// Returns `true` if the expression is [`UnaryOperation`].
    ///
    /// [`UnaryOperation`]: Expression::UnaryOperation
    #[must_use]
    pub fn is_unary_operation(&self) -> bool {
        matches!(self, Self::UnaryOperationLeft(..))
    }

    pub fn as_unary_operation(&self) -> Option<&UnaryOperation<'a>> {
        if let Self::UnaryOperationLeft(v) = self {
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
        let (s, mut result) = alt((
            map(UnaryOperation::parse_span, Self::UnaryOperationLeft),
            //map(FunctionCall::parse_span, Self::FunctionCall),
            //map(BinaryOperation::parse_span, Self::BinaryOperation),
            map(BracketOperation::parse_span, Self::BracketOperation),
            map(Continue::parse_span, Self::Continue),
            map(Break::parse_span, Self::Break),
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(CharLiteral::parse_span, Self::CharLiteral),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)?;
        let mut next_s = s;
        loop {
            let s_ref = &next_s;
            let extend =
                move |expr: &mut Expression<'a>, child_res| expr.extend::<E>(s_ref, child_res);
            //F: Fn(&mut Expression<'a>, Option<R>) -> R,
            match result.visit_rhs_mod_travel_up(&extend)? {
                ExtendSelfOk::Applied(s) => next_s = s,
                ExtendSelfOk::NotApplied((s, ext)) => {
                    result = match ext {
                        ExpressionExtension::FunctionCall(params) => {
                            FunctionCall::new(Box::new(result), params).into()
                        }
                        ExpressionExtension::BinaryOperation(operator, operant) => {
                            BinaryOperation::new(
                                Box::new(result),
                                operator,
                                Box::new(operant.into()),
                            )
                            .into()
                        }
                        ExpressionExtension::Deref(at) => Deref::new(Box::new(result), at).into(),
                        ExpressionExtension::Address(exclamation_mark) => {
                            Address::new(Box::new(result), exclamation_mark).into()
                        }
                    };
                    next_s = s;
                }
                ExtendSelfOk::NothingToApply(s) => return Ok((s, result)),
            }
        }
    }
}

/// contrary to [FromSpan] this trait is used for structs/ enums where it is not obvious, which
/// type they have after the first valid option was read, for example `f(a)` could be read as the
/// [IdentPath] `f` first, but when continuing to read, it is clear, that it is a FunctionCall
trait ExtendFromSpan<'a>
where
    Self: Sized,
{
    type Output: Sized;

    /// takes the current self and tries to convert it into a super-object by adding information
    /// from the passed span.
    /// e.g. when you call this on an expression that is an [Expression::IdentPath] with `()`
    /// it will become an [Expression::FunctionCall] on that IdentPath
    ///
    //note that self is passed as reference, this is to not have to return it in case the attempt
    //failed. This means that on success self will need to be cloned. However as self would have to
    //have been moved anyway this should not have a significant performance impact (though I did
    //not check this, so measure in case of issues)
    fn extend_from_span<E>(&self, s: Span<'a>) -> IResult<Span, Self::Output, E>
    where
        E: SpanParseError<'a>;
}

impl<'a> ExtendFromSpan<'a> for Expression<'a> {
    type Output = Self;

    fn extend_from_span<E>(&self, s: Span<'a>) -> IResult<Span, Self::Output, E>
    where
        E: SpanParseError<'a>,
    {
        // a+b *c+d
        // a
        //  - no takers -> add new root
        // a+b
        //  ^
        // a+(b*c)
        //  - no takers -> add new root
        // (a+(b*c))+d
        //
        //
        // a = b = 7
        // a
        //  - no takers -> add new root
        // a = b
        //   ^
        // a = ( b = 7 )
        todo!()
    }
}

impl<'a> ExtendFromSpan<'a> for IdentPath<'a> {
    type Output = Expression<'a>;

    fn extend_from_span<E>(&self, s: Span<'a>) -> IResult<Span<'a>, Self::Output, E>
    where
        E: SpanParseError<'a>,
    {
        /*map(FunctionCallParameters::parse_span, |parameters| {
            Expression::FunctionCall(FunctionCall {
                callee: FunctionCallee::IdentPath(self.clone()),
                parameters,
            })
        })(s)*/
        todo!()
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
