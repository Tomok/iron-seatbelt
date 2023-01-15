use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{ContextError, ParseError},
    IResult,
};
use nom_locate::LocatedSpan;
use paste::paste;

use super::{space_or_comment0, CharLiteral, FromSpan, IdentPath, IntLiteral, Span};

mod function_call;
use function_call::FunctionCall;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression<'a> {
    IntLiteral(IntLiteral<'a>),
    CharLiteral(CharLiteral<'a>),
    /// an Ident, could be a variable_name or a function name
    IdentPath(IdentPath<'a>),
    FunctionCall(FunctionCall<'a>),
    //todo operators
}

impl<'a> FromSpan<'a> for Expression<'a> {
    fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
        s: Span<'a>,
    ) -> IResult<Span, Self, E> {
        alt((
            map(IntLiteral::parse_span, Self::IntLiteral),
            map(CharLiteral::parse_span, Self::CharLiteral),
            map(FunctionCall::parse_span, Self::FunctionCall),
            map(IdentPath::parse_span, Self::IdentPath),
        ))(s)
    }
}

pub trait BinaryOperation<'a> {
    type Left;
    type Right;
    /// left hand side of the operation
    fn lhs(&self) -> &Self::Left;
    /// right hand side of the operation
    fn rhs(&self) -> &Self::Right;
    fn operant(&self) -> &Span<'a>;
    fn operator() -> &'static str;
    /// get a mutable tuple of both sides of the operation
    fn get_mut(&mut self) -> (&mut Self::Left, &mut Self::Right);
}

macro_rules! BinaryOperation {
    ($id: ident<$t: lifetime> {$left: ty, [ $($op: literal : $op_id: ident,)+ ], $right: ty}) => {
        #[derive(PartialEq, Eq, Debug, Clone)]
        pub enum $id<$t> {
            $(
                $op_id($op_id<$t>),
            )+
        }

        impl<$t> $id<$t> {
            #[must_use]
            pub fn lhs(&self) -> &Box<$left> {
                match self {
                    $(
                        Self::$op_id(inner) => inner.lhs(),
                    )+
                }
            }

            #[must_use]
            pub fn rhs(&self) -> &$right {
                match self {
                    $(
                        Self::$op_id(inner) => inner.rhs(),
                    )+
                }
            }

            #[must_use]
            pub fn operant(&self) -> &Span<'a> {
                match self {
                    $(
                        Self::$op_id(inner) => inner.operant(),
                    )+
                }
            }

            #[must_use]
            pub fn operator(&self) -> &'static str {
                match self {
                    $(
                        Self::$op_id(_) => <$op_id>::operator(),
                    )+
                }
            }

            #[must_use]
            pub fn get_mut(&mut self) -> (&mut Box<$left>, &mut $right) {
                match self {
                    $(
                        Self::$op_id(inner) => inner.get_mut(),
                    )+
                }
            }
        }


        $(
            #[derive(PartialEq, Eq, Debug, Clone)]
            pub struct $op_id<$t> {
                pub lhs: Box<$left>,
                pub operant: Span<$t>,
                pub rhs: $right,
            }

            impl<'a> BinaryOperation<'a> for $op_id<'a> {
                type Left = Box<$left>;

                type Right = $right;

                fn lhs(&self) -> &Self::Left {
                    &self.lhs
                }

                fn rhs(&self) -> &Self::Right {
                    &self.rhs
                }

                fn operant(&self) -> &Span<'a> {
                    &self.operant
                }

                fn operator() -> &'static str {
                    $op
                }

                fn get_mut(&mut self) -> (&mut Self::Left, &mut Self::Right) {
                    (&mut self.lhs, &mut self.rhs)
                }
            }
        )+

        impl<'a> FromSpan<'a> for $id<'a> {
            fn parse_span<
                E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>,
            >(
                s: Span<'a>,
            ) -> IResult<Span, Self, E> {
                enum Operators<'a>{
                    $(
                        $op_id(Span<'a>),
                    )+
                }
                let (s, lhs) = <$left>::parse_span(s)?;
                let lhs = Box::new(lhs);
                let (s, _) = space_or_comment0(s)?;
                let (s, op) = alt((
                    $(
                        map(tag($op), |operant| Operators::$op_id(operant)),
                    )+
                ))(s)?;
                let (s, _) = space_or_comment0(s)?;
                let (s, rhs) = <$right>::parse_span(s)?;
                Ok((s, match op {
                    $(
                        Operators::$op_id(operant) => Self::$op_id($op_id{lhs, operant, rhs}),
                    )+
                }))
            }
        }

    };
}

BinaryOperation!(AddSub<'a> {AdditionOperantLhs<'a>, ["+": Addition, "-": Substraction,], AdditionOperant<'a>});
BinaryOperation!(MulDivMod<'a> {MultiplicationOperant<'a>, ["*": Multiplication, "/": Division, "mod": Modulo,], MultiplicationOperant<'a>});

macro_rules! Operant {
    (pub enum $id: ident<$t: lifetime> {
        $($variant_ident:ident ($variant_type: ty),)+
    }) => {
        #[derive(PartialEq, Eq, Debug, Clone)]
        pub enum $id<$t> {
            $($variant_ident($variant_type)),+
        }

        impl<'a> FromSpan<'a> for $id<'a> {
            fn parse_span<E: ParseError<LocatedSpan<&'a str>> + ContextError<LocatedSpan<&'a str>>>(
                s: Span<'a>,
            ) -> IResult<Span, Self, E> {
                alt((
                    $(map(<$variant_type>::parse_span, Self::$variant_ident)),+
                ))(s)
            }
        }

        impl<'a> $id<'a> {
            paste!{
                $(
                    #[must_use]
                    pub fn [<as_ $variant_ident:snake>](&self) -> Option<&$variant_type> {
                        if let Self::$variant_ident(inner) = self {
                            Some(&inner)
                        } else {
                            None
                        }
                    }

                    #[must_use]
                    pub fn [<is_ $variant_ident:snake>](&self) -> bool {
                        if let Self::$variant_ident(_) = self {
                            true
                        } else {
                            false
                        }
                    }
                )+
            }
        }

    }
}

Operant!(
    pub enum AdditionOperant<'a> {
        MulDivMod(MulDivMod<'a>),
        FunctionCall(FunctionCall<'a>),
        IntLiteral(IntLiteral<'a>),
        IdentPath(IdentPath<'a>),
        //TODO shift operations, bitwise operations
    }
);
Operant!(
    pub enum AdditionOperantLhs<'a> {
        AddSub(AddSub<'a>),
        MulDivMod(MulDivMod<'a>),
        FunctionCall(FunctionCall<'a>),
        IntLiteral(IntLiteral<'a>),
        IdentPath(IdentPath<'a>),
        //TODO shift operations, bitwise operations
    }
);

Operant!(
    pub enum MultiplicationOperant<'a> {
        FunctionCall(FunctionCall<'a>),
        IntLiteral(IntLiteral<'a>),
        IdentPath(IdentPath<'a>),
    }
);

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
        let (_, res) = all_consuming::<_, _, VerboseError<_>, _>(AddSub::parse_span)(span).unwrap();
        assert_eq!(operator, res.operator());
        assert_eq!("a", &res.lhs().as_ident_path().unwrap().to_string());
        assert_eq!("b", &res.rhs().as_ident_path().unwrap().to_string());
    }

    #[test]
    fn test_complex_arithmetic() {
        let input = "a + b * c - c";
        let span = LocatedSpan::new(input);
        let (_, res) = all_consuming::<_, _, VerboseError<_>, _>(AddSub::parse_span)(span).unwrap();
        dbg!(res);
        panic!()
    }
}
