use nom::{
    bytes::complete::tag,
    combinator::{map, opt},
    error::{ContextError, ParseError},
    multi::{many0, many1},
    sequence::{separated_pair, terminated},
    IResult,
};
use nom_locate::LocatedSpan;

use super::{space_or_comment0, Expression, FromSpan, IdentPath, Span};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionCall<'a> {
    callee: FunctionCallee<'a>,
    //that could be used?
    parameters: FunctionCallParameters<'a>,
}

impl<'a> From<FunctionCallChain<'a>> for FunctionCall<'a> {
    fn from(value: FunctionCallChain<'a>) -> Self {
        let (innermost_params, call_params) = value.call_parameters.split_first().unwrap();
        // unwrap is safe, as [FunctionCallChain.call_parameters] guarantees at least one element

        let mut current_outermost_call = FunctionCall {
            callee: FunctionCallee::IdentPath(value.innermost_function_ident),
            parameters: innermost_params.clone(),
        };

        for current_call_params in call_params.iter() {
            current_outermost_call = FunctionCall {
                callee: FunctionCallee::FunctionCall(Box::new(current_outermost_call)),
                parameters: current_call_params.clone(),
            };
        }
        current_outermost_call
    }
}

impl<'a> FromSpan<'a> for FunctionCall<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, call_chain) = FunctionCallChain::parse_span(s)?;
        Ok((s, call_chain.into()))
    }
}

/// this is a helper for calls on function call return values (e.g. `f()()`), without this [functionCall::parse_span] would
/// have to call itself to check for this, leading to infinite recursion
///
/// instead using this, find the identifier first (`f` in the example ) and than a chain of calls
/// (`vec![(), ()]` in the example). that can than be converted into the corresponding functionCall
/// objects
#[derive(Debug)]
struct FunctionCallChain<'a> {
    innermost_function_ident: IdentPath<'a>,
    /// call_parameters, has at least one value, otherwise this is not a function call chain but an
    /// [IdentPath], hence this object should not have been created
    call_parameters: Vec<FunctionCallParameters<'a>>,
}

impl<'a> FromSpan<'a> for FunctionCallChain<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, innermost_function_ident) = IdentPath::parse_span(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, call_parameters) = many1(FunctionCallParameters::parse_span)(s)?;
        Ok((
            s,
            Self {
                innermost_function_ident,
                call_parameters,
            },
        ))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum FunctionCallee<'a> {
    IdentPath(IdentPath<'a>),
    FunctionCall(Box<FunctionCall<'a>>),
}

impl<'a> FunctionCallee<'a> {
    pub fn as_ident_path(&self) -> Option<&IdentPath<'a>> {
        if let Self::IdentPath(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the function callee is [`IdentPath`].
    ///
    /// [`IdentPath`]: FunctionCallee::IdentPath
    #[must_use]
    pub fn is_ident_path(&self) -> bool {
        matches!(self, Self::IdentPath(..))
    }

    /// Returns `true` if the function callee is [`FunctionCall`].
    ///
    /// [`FunctionCall`]: FunctionCallee::FunctionCall
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
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionCallParameters<'a> {
    open_brace: Span<'a>,
    parameters: Vec<FunctionCallParameter<'a>>,
    closing_brace: Span<'a>,
}

impl<'a> FromSpan<'a> for FunctionCallParameters<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        let (s, open_brace) = tag("(")(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, parameters) = many0(FunctionCallParameter::parse_span)(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, closing_brace) = tag(")")(s)?;
        Ok((
            s,
            Self {
                open_brace,
                parameters,
                closing_brace,
            },
        ))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionCallParameter<'a> {
    expression: Box<Expression<'a>>,
    comma: Option<Span<'a>>,
}

impl<'a> FromSpan<'a> for FunctionCallParameter<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
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
    use nom::{combinator::all_consuming, error::VerboseError};

    #[test]
    fn simple_function_call() {
        let input = "f()";
        let span = LocatedSpan::new(input);
        let (_s, res) =
            all_consuming::<_, _, VerboseError<_>, _>(FunctionCall::parse_span)(span).unwrap();
        assert!(
            res.parameters.parameters.is_empty(),
            "Expected no parameters, but found: {:#?}",
            res.parameters
        );
        assert_eq!("f", &res.callee.as_ident_path().unwrap().to_string());
    }

    #[test]
    fn chained_function_call() {
        let input = "f()(1)";
        let span = LocatedSpan::new(input);
        let (_s, res) =
            all_consuming::<_, _, VerboseError<_>, _>(FunctionCall::parse_span)(span).unwrap();
        assert_eq!(
            1,
            res.parameters.parameters.len(),
            "Expected one parameter, but found: {:#?}",
            res.parameters
        );
        let inner_function = res.callee.as_function_call().unwrap();
        assert_eq!(
            "f",
            &inner_function.callee.as_ident_path().unwrap().to_string()
        );
        assert!(
            inner_function.parameters.parameters.is_empty(),
            "Expected no parameters, but found: {:#?}",
            inner_function.parameters
        );
    }
}
