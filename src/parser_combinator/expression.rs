use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    error::{ContextError, ParseError},
    multi::{many0, many1},
    sequence::{separated_pair, terminated},
    IResult,
};
use nom_locate::LocatedSpan;

use super::{space_or_comment0, CharLiteral, IdentPath, IntLiteral, Span};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression<'s> {
    IntLiteral(IntLiteral<'s>),
    CharLiteral(CharLiteral<'s>),
    BinaryOperator(BinaryOperator<'s>),
    /// an Ident, could be a variable_name or a function name
    IdentPath(IdentPath<'s>),
    FunctionCall(FunctionCall<'s>),
}

impl<'s> Expression<'s> {
    pub fn parse_span<E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        s: Span<'s>,
    ) -> IResult<Span, Self, E> {
        let (remaining, tokens) = many1(ExpressionToken::parse_span)(s)?;
        let expr = Expression::from_tokens(tokens.as_slice())?;
        Ok((remaining, expr))
    }

    fn from_tokens<'v, E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        values: &'v [ExpressionToken<'s>],
    ) -> Result<Self, nom::Err<E>> {
        if let [value] = values {
            match value {
                ExpressionToken::IntLiteral(v) => Ok(Self::IntLiteral(v.clone())),
                ExpressionToken::CharLiteral(v) => Ok(Self::CharLiteral(v.clone())),
                ExpressionToken::Braces(_) => todo!(),
                ExpressionToken::Operator(_) => todo!(),
                ExpressionToken::IdentPath(v) => Ok(Self::IdentPath(v.clone())),
            }
        } else {
            //more than one token ... need to build tree
            // check if this is a function - for function calls the last call is the outermost
            if let Some((maybe_params, function_expr_tokens)) = values.split_last() {
                let maybe_params: &'v _ = maybe_params;
                let function_expr_tokens: &'v _ = function_expr_tokens;
                if let Some(maybe_params) = maybe_params.as_braces() {
                    let maybe_params: &'v _ = maybe_params;
                    let call =
                        FunctionCall::from_expr_and_braces(function_expr_tokens, maybe_params)?;
                    return Ok(Expression::FunctionCall(call));
                }
            }

            if let Some(op) = BinaryOperator::from_tokens(values)? {
                return Ok(Expression::BinaryOperator(op));
            }
            todo!()
        }
    }

    pub fn as_ident_path(&self) -> Option<&IdentPath<'s>> {
        if let Self::IdentPath(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

///due to possible recursion, e.g. `a()()` it is easier to parse expressions into tokens first
///and then generate the AST based on that
#[derive(PartialEq, Eq, Debug, Clone)]
enum ExpressionToken<'s> {
    IntLiteral(IntLiteral<'s>),
    CharLiteral(CharLiteral<'s>),
    Braces(BracesTokens<'s>),
    Operator(OperatorToken<'s>),
    /// an Ident, could be a variable_name or a function name
    IdentPath(IdentPath<'s>),
}

impl<'s> ExpressionToken<'s> {
    pub fn parse_span<E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        s: Span<'s>,
    ) -> IResult<Span, Self, E> {
        terminated(
            alt((
                map(IntLiteral::parse_span, Self::IntLiteral),
                map(CharLiteral::parse_span, Self::CharLiteral),
                map(BracesTokens::parse_span, Self::Braces),
                map(OperatorToken::parse_span, Self::Operator),
                map(IdentPath::parse_span, Self::IdentPath),
            )),
            space_or_comment0,
        )(s)
    }

    fn as_operator(&self) -> Option<&OperatorToken<'s>> {
        if let Self::Operator(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    fn is_comma_operator(&self) -> bool {
        matches!(self, Self::Operator(OperatorToken::Comma(_)))
    }

    #[must_use]
    fn as_comma_operator<'a>(&'a self) -> Option<&'a Span<'s>> {
        self.as_operator()
            .filter(|t| t.is_comma())
            .map(OperatorToken::span)
    }

    fn as_braces(&self) -> Option<&BracesTokens<'s>> {
        if let Self::Braces(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct BracesTokens<'s> {
    open_brace: Span<'s>,
    contents: Vec<ExpressionToken<'s>>,
    closing_brace: Span<'s>,
}

impl<'s> BracesTokens<'s> {
    pub fn parse_span<E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        s: Span<'s>,
    ) -> IResult<Span, Self, E> {
        let (s, open_brace) = tag("(")(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, contents) = many0(ExpressionToken::parse_span)(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, closing_brace) = tag(")")(s)?;
        Ok((
            s,
            Self {
                open_brace,
                contents,
                closing_brace,
            },
        ))
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum OperatorToken<'s> {
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

    Comma(Span<'s>),
}

impl<'s> PartialOrd for OperatorToken<'s> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        const fn operator_to_bind_strength(o: &OperatorToken<'_>) -> i8 {
            match o {
                OperatorToken::Mul(_) => 4,
                OperatorToken::Div(_) => 4,
                OperatorToken::Mod(_) => 4,
                OperatorToken::Add(_) => 3,
                OperatorToken::Sub(_) => 3,
                OperatorToken::And(_) => 2,
                OperatorToken::Or(_) => 2,
                OperatorToken::Eq(_) => 1,
                OperatorToken::Neq(_) => 1,
                OperatorToken::Le(_) => 1,
                OperatorToken::Ge(_) => 1,
                OperatorToken::Lt(_) => 1,
                OperatorToken::Gt(_) => 1,
                OperatorToken::Assign(_) => 0,
                OperatorToken::Comma(_) => -1,
            }
        }

        operator_to_bind_strength(self).partial_cmp(&operator_to_bind_strength(other))
    }
}

impl<'s> OperatorToken<'s> {
    pub fn parse_span<E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
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
            map(tag(","), Self::Comma),
        ))(s)
    }

    #[must_use]
    pub fn span<'a>(&'a self) -> &'a Span<'s> {
        match self {
            OperatorToken::Add(s)
            | OperatorToken::Sub(s)
            | OperatorToken::Mul(s)
            | OperatorToken::Div(s)
            | OperatorToken::Mod(s)
            | OperatorToken::And(s)
            | OperatorToken::Or(s)
            | OperatorToken::Eq(s)
            | OperatorToken::Neq(s)
            | OperatorToken::Le(s)
            | OperatorToken::Ge(s)
            | OperatorToken::Lt(s)
            | OperatorToken::Gt(s)
            | OperatorToken::Assign(s)
            | OperatorToken::Comma(s) => s,
        }
    }

    /// Returns `true` if the operator token is [`Add`].
    ///
    /// [`Add`]: OperatorToken::Add
    #[must_use]
    pub fn is_add(&self) -> bool {
        matches!(self, Self::Add(..))
    }

    /// Returns `true` if the operator token is [`Sub`].
    ///
    /// [`Sub`]: OperatorToken::Sub
    #[must_use]
    pub fn is_sub(&self) -> bool {
        matches!(self, Self::Sub(..))
    }

    /// Returns `true` if the operator token is [`Mul`].
    ///
    /// [`Mul`]: OperatorToken::Mul
    #[must_use]
    pub fn is_mul(&self) -> bool {
        matches!(self, Self::Mul(..))
    }

    /// Returns `true` if the operator token is [`Div`].
    ///
    /// [`Div`]: OperatorToken::Div
    #[must_use]
    pub fn is_div(&self) -> bool {
        matches!(self, Self::Div(..))
    }

    /// Returns `true` if the operator token is [`Mod`].
    ///
    /// [`Mod`]: OperatorToken::Mod
    #[must_use]
    pub fn is_mod(&self) -> bool {
        matches!(self, Self::Mod(..))
    }

    /// Returns `true` if the operator token is [`Or`].
    ///
    /// [`Or`]: OperatorToken::Or
    #[must_use]
    pub fn is_or(&self) -> bool {
        matches!(self, Self::Or(..))
    }

    /// Returns `true` if the operator token is [`Eq`].
    ///
    /// [`Eq`]: OperatorToken::Eq
    #[must_use]
    pub fn is_eq(&self) -> bool {
        matches!(self, Self::Eq(..))
    }

    /// Returns `true` if the operator token is [`Neq`].
    ///
    /// [`Neq`]: OperatorToken::Neq
    #[must_use]
    pub fn is_neq(&self) -> bool {
        matches!(self, Self::Neq(..))
    }

    /// Returns `true` if the operator token is [`Comma`].
    ///
    /// [`Comma`]: OperatorToken::Comma
    #[must_use]
    fn is_comma(&self) -> bool {
        matches!(self, Self::Comma(..))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct BinaryOperator<'s> {
    lhs: Box<Expression<'s>>,
    operator: OperatorToken<'s>,
    rhs: Box<Expression<'s>>,
}

impl<'s> BinaryOperator<'s> {
    fn from_tokens<'v, E>(tokens: &'v [ExpressionToken<'s>]) -> Result<Option<Self>, nom::Err<E>>
    where
        E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
    {
        //find the leftmost weakest operator and split there
        let mut weakest_operator = None;
        for (index, token) in tokens.iter().enumerate() {
            if let ExpressionToken::Operator(operator) = token {
                if weakest_operator
                    .map(|(_, wo)| wo < operator)
                    .unwrap_or(true)
                {
                    weakest_operator = Some((index, operator));
                }
            }
        }
        if let Some((index, operator)) = weakest_operator {
            let (lhs, op_and_rhs) = tokens.split_at(index);
            if lhs.is_empty() || op_and_rhs.len() < 2 {
                // operator found, but lacking lhs or rhs
                return Ok(None);
            }
            let rhs = &op_and_rhs[1..];
            Ok(Some(Self {
                lhs: Box::new(Expression::from_tokens(lhs)?),
                operator: Clone::clone(operator),
                rhs: Box::new(Expression::from_tokens(rhs)?),
            }))
        } else {
            // no operators found
            Ok(None)
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionCall<'s> {
    callee: Box<Expression<'s>>, //does this make sense? is there a more specific type,
    //that could be used?
    parameters: FunctionCallParameters<'s>,
}

impl<'s> FunctionCall<'s> {
    fn from_expr_and_braces<'v, E>(
        function_expr_tokens: &'v [ExpressionToken<'s>],
        maybe_params: &'v BracesTokens<'s>,
    ) -> Result<FunctionCall<'s>, nom::Err<E>>
    where
        E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
    {
        let parameters = FunctionCallParameters::from_tokens(maybe_params)?;
        let callee = Box::new(Expression::from_tokens(function_expr_tokens)?);
        Ok(Self { callee, parameters })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionCallParameters<'s> {
    open_brace: Span<'s>,
    parameters: Vec<FunctionCallParameter<'s>>,
    closing_brace: Span<'s>,
}

impl<'s> FunctionCallParameters<'s> {
    fn from_tokens<'v, E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        value: &'v BracesTokens<'s>,
    ) -> Result<Self, nom::Err<E>> {
        let open_brace = value.open_brace;
        let closing_brace = value.closing_brace;
        let mut parameters = Vec::new();
        for param in value
            .contents
            .split_inclusive(ExpressionToken::is_comma_operator)
        {
            parameters.push(FunctionCallParameter::from_tokens(param)?);
        }
        Ok(Self {
            open_brace,
            parameters,
            closing_brace,
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionCallParameter<'s> {
    expression: Box<Expression<'s>>,
    comma: Option<Span<'s>>,
}

impl<'s> FunctionCallParameter<'s> {
    fn from_tokens<'v, E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        value: &'v [ExpressionToken<'s>],
    ) -> Result<Self, nom::Err<E>> {
        assert!(!value.is_empty()); //should only be called, if there is a parameter
        if let Some((last, expression_tokens)) = value.split_last() {
            let expression_tokens: &'v _ = expression_tokens;
            if let Some(comma) = last.as_comma_operator() {
                let expression = Box::new(Expression::from_tokens(expression_tokens)?);
                Ok(Self {
                    expression,
                    comma: Some(Clone::clone(comma)),
                })
            } else {
                let expression = Box::new(Expression::from_tokens(value)?);
                Ok(Self {
                    expression,
                    comma: None,
                })
            }
        } else {
            // only one element
            let expression = Box::new(Expression::from_tokens(value)?);
            Ok(Self {
                expression,
                comma: None,
            })
        }
    }
}

impl<'s> FunctionCallParameter<'s> {
    pub fn parse_span<E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>>(
        s: Span<'s>,
    ) -> IResult<Span, Self, E> {
        terminated(
            map(
                separated_pair(Expression::parse_span, space_or_comment0, opt(tag(","))),
                |(expr, comma)| Self {
                    expression: Box::new(expr),
                    comma,
                },
            ),
            space_or_comment0,
        )(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{combinator::eof, error::VerboseError};

    #[test]
    fn test_operators() {
        let input = "b == c";
        let span = LocatedSpan::new(input);
        let (remaining, tokens) =
            terminated(many1(ExpressionToken::parse_span::<VerboseError<_>>), eof)(span).unwrap();
        let parsed = BinaryOperator::from_tokens::<VerboseError<_>>(tokens.as_slice())
            .unwrap()
            .unwrap();
        let operator_token = parsed.operator;
        assert!(matches!(operator_token, OperatorToken::Eq(_)));
        let lhs = parsed.lhs.as_ident_path().unwrap();
        assert!(lhs.segments.len() == 1);
        let lhs_ident = &lhs.segments[0];
        assert_eq!("b", <&str>::from(&lhs_ident.ident));
        let rhs = parsed.rhs.as_ident_path().unwrap();
        assert!(rhs.segments.len() == 1);
        let rhs_ident = &rhs.segments[0];
        assert_eq!("c", <&str>::from(&rhs_ident.ident));
    }
}
