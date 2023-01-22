use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while1},
    character::{
        self,
        complete::{line_ending, multispace0, multispace1, space0, space1},
    },
    combinator::{eof, map, opt, peek},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
    AsChar,
};
use paste::paste;

use crate::parser_combinator::{space_or_comment0, Comment, StringLiteral};

use super::{FromSpan, Ident, IntLiteral, Span, SpanParseError};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BssemblyBlock<'a> {
    bsm: Span<'a>,
    open_curly_brace: Span<'a>,
    code: BssemblyCode<'a>,
    closing_curly_brace: Span<'a>,
}

impl<'a> FromSpan<'a> for BssemblyBlock<'a> {
    fn parse_span<E: SpanParseError<'a>>(s: Span<'a>) -> nom::IResult<Span, Self, E> {
        let (s, bsm) = tag("bsm")(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, open_curly_brace) = tag("{")(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, code) = BssemblyCode::parse_span(s)?;
        let (s, _) = space_or_comment0(s)?;
        let (s, closing_curly_brace) = tag("}")(s)?;
        Ok((
            s,
            Self {
                bsm,
                open_curly_brace,
                code,
                closing_curly_brace,
            },
        ))
    }
}

/// a sequence of [BssemblyExpression]s when parsing this supports comments, but they are not
/// preserved
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BssemblyCode<'a> {
    expressions: Vec<BssemblyExpression<'a>>,
}

fn comment_or_line_end_or_eof<'a, E>(s: Span<'a>) -> nom::IResult<Span<'a>, (), E>
where
    E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
        + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
{
    alt((
        map(Comment::parse_span, |_| ()),
        map(line_ending, |_| ()),
        map(eof, |_| ()),
    ))(s)
}

impl<'a> FromSpan<'a> for BssemblyCode<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, _) = space_or_comment0(s)?;
        let (s, expressions) = many0(terminated(
            BssemblyExpression::parse_span,
            delimited(space0, comment_or_line_end_or_eof, multispace0),
        ))(s)?;
        Ok((s, Self { expressions }))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BssemblyExpression<'a> {
    Label(Label<'a>),
    StringDef(StringDef<'a>),
    Operation(Operation<'a>),
}

impl<'a> FromSpan<'a> for BssemblyExpression<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        alt((
            map(Label::parse_span, Self::Label),
            map(StringDef::parse_span, Self::StringDef),
            map(Operation::parse_span, Self::Operation),
        ))(s)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Label<'a> {
    identifier: Ident<'a>,
    colon: Span<'a>,
}

impl<'a> FromSpan<'a> for Label<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, identifier) = Ident::parse_span(s)?;
        let (s, _) = many0(space1)(s)?;
        let (s, colon) = tag(":")(s)?;
        Ok((s, Self { identifier, colon }))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct StringDef<'a> {
    dot_string: Span<'a>,
    value: StringLiteral<'a>,
}

impl<'a> FromSpan<'a> for StringDef<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, dot_string) = tag(".string")(s)?;
        let (s, _) = many0(space1)(s)?;
        let (s, value) = StringLiteral::parse_span(s)?;
        Ok((s, Self { dot_string, value }))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Register<'a> {
    R(u8), //todo include Spans
    SP(Span<'a>),
    IP(Span<'a>),
}

impl<'a> FromSpan<'a> for Register<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        alt((
            map(tag_no_case("SP"), Self::SP),
            map(tag_no_case("IP"), Self::IP),
            map(preceded(tag_no_case("R"), character::complete::u8), Self::R),
        ))(s)
    }
}

impl std::fmt::Display for Register<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::R(i) => write!(f, "R{i}"),
            Self::SP(_) => write!(f, "SP"),
            Self::IP(_) => write!(f, "IP"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// a single operation
pub struct Operation<'a> {
    opcode: Opcode<'a>,
    parameters: Vec<OperationParameterEntry<'a>>,
}

impl<'a> FromSpan<'a> for Operation<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, opcode) = Opcode::parse_span(s)?;
        let mut parameters = Vec::with_capacity(3);
        let mut next_s = s;
        let mut first_pass = true;
        loop {
            let (s, parameter) = if first_pass {
                //in the first pass it is not mandatory to have a
                //parameter, in all following it is
                opt(preceded(many1(space1), OperationParameter::parse_span))(next_s)?
            } else {
                let (s, _) = many0(space1)(next_s)?;
                let (s, p) = OperationParameter::parse_span(s)?;
                (s, Some(p))
            };
            let (s, _) = many0(space1)(s)?;
            if let Some(parameter) = parameter {
                let (s, following_colon) = opt(tag(","))(s)?;
                next_s = s;
                let no_colon = following_colon.is_none();
                parameters.push(OperationParameterEntry {
                    parameter,
                    following_colon,
                });
                if no_colon {
                    break;
                }
            } else {
                next_s = s;
                assert!(first_pass);
                break;
            }
            first_pass = false;
        }
        let s = next_s;
        Ok((s, Self { opcode, parameters }))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct OperationParameterEntry<'a> {
    parameter: OperationParameter<'a>,
    following_colon: Option<Span<'a>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum OperationParameter<'a> {
    Register(Register<'a>),
    PointerRegister(PointerRegister<'a>),
    Value(Value<'a>),
    Address(Address<'a>),
}

impl<'a> FromSpan<'a> for OperationParameter<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        alt((
            map(Register::parse_span, Self::Register),
            map(PointerRegister::parse_span, Self::PointerRegister),
            map(Value::parse_span, Self::Value),
            map(Address::parse_span, Self::Address),
        ))(s)
    }
}

type Value<'a> = IntLiteral<'a>;
type Address<'a> = Ident<'a>;

#[derive(PartialEq, Eq, Clone, Debug)]
/// a register used to point to a memory location
pub struct PointerRegister<'a> {
    pointer_character: Span<'a>,
    register: Register<'a>,
}

impl<'a> FromSpan<'a> for PointerRegister<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, pointer_character) = tag("*")(s)?;
        let (s, _) = space0(s)?; //not sure if whitespace is allowed, could not find an
                                 //example of this being used, but should not hurt
        let (s, register) = Register::parse_span(s)?;
        Ok((
            s,
            Self {
                pointer_character,
                register,
            },
        ))
    }
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct InvalidOpcode<'a>(Span<'a>);

impl<'a, E> From<InvalidOpcode<'a>> for nom::Err<E>
where
    E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
        + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
{
    fn from(value: InvalidOpcode<'a>) -> Self {
        use nom::error::*;
        let e = E::from_error_kind(value.0, ErrorKind::Alt);
        nom::Err::Error(e)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum InvalidCommand<'a> {
    UnknownParameterSet(Operation<'a>),
}

impl<'a, E> From<InvalidCommand<'a>> for nom::Err<E>
where
    E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
        + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
{
    fn from(value: InvalidCommand<'a>) -> Self {
        use nom::error::*;
        //todo: add context? or alternatives via E::or
        match value {
            InvalidCommand::UnknownParameterSet(value) => {
                let e = E::from_error_kind(*value.opcode.span(), ErrorKind::Verify);
                let e = E::add_context(
                    *value.opcode.span(), //todo idealy this should span value.parameters
                    "Unkown Parameters for bssembly operation",
                    e,
                );
                nom::Err::Error(e)
            }
        }
    }
}

/// takes all command definitions in the form `<command> VARIANTS { <VariantName> {<StructLike field definitions>},}`
/// and defines
/// * a [Command] enum containing them
/// * an enum per `command` containing the VARIANTS
/// * one struct per `VariantName` with the defined fields
/// and implements [FromSpan] for command and [todo] for other structs
macro_rules! commands {
    ($(
        $cmd:ident VARIANTS { $(
                $variant_name:ident { $(
                    $param_name:ident : $param_type:ty
                ),*} $comment:literal
        ,)+},
    )+) => {
        paste!{
            $($(
                #[derive(PartialEq, Eq, Clone, Debug)]
                #[doc = $comment]
                pub struct [<$variant_name:camel>]<'a> {
                    /// the [Span] containing the command itself (without any parameters)
                    pub opcode: Opcode<'a>,
                    $(
                        pub $param_name: $param_type<'a>,
                    )*
                }

            )+)+
            $(
                #[derive(PartialEq, Eq, Clone, Debug)]
                pub enum [<$cmd:camel>]<'a> {$(
                    [<$variant_name:camel>]([<$variant_name:camel>]<'a>),
                )+}
            )+

            #[derive(PartialEq, Eq, Clone, Debug)]
            pub enum Command<'a> {$(
                [<$cmd:camel>]([<$cmd:camel>]<'a>),
            )+}

            #[derive(PartialEq, Eq, Clone, Debug)]
            pub enum Opcode<'a> {$(
                [<$cmd:camel>](Span<'a>),
            )+}

            impl<'a> Opcode<'a>{
                pub fn span<'b>(&'b self) -> &'b Span<'a> {
                    match self {$(
                        Self::[<$cmd:camel>](s) => s,
                    )+}
                }
            }

            impl<'a> TryFrom<Span<'a>> for Opcode<'a> {
                type Error = InvalidOpcode<'a>;

                fn try_from(value: Span<'a>) -> Result<Self, Self::Error> {
                    match value.fragment().to_lowercase().as_str() {
                        $(stringify!([<$cmd:lower>]) => Ok(Self::[<$cmd:camel>](value)),)+
                        _ => Err(InvalidOpcode(value)),
                    }
                }
            }

            impl<'a> TryFrom<Operation<'a>> for Command<'a>  {
                type Error = InvalidCommand<'a>;

                fn try_from(value: Operation<'a>) -> Result<Self, Self::Error> {
                    type Ope<'o> = OperationParameterEntry<'o>;
                    use OperationParameter::*;
                    let opcode = &value.opcode;
                    match (opcode, value.parameters.as_slice()) {
                        /* to generate: (
                            Opcode::Copy(_),
                            [Ope {
                                parameter: Value(value),
                                ..
                            }, Ope {
                                parameter: Register(target),
                                ..
                            }],
                        ) => Ok(Command::Copy(Copy::MoveRegisterImmediate(
                            MoveRegisterImmediate {
                                opcode: opcode.clone(),
                                value: value.clone(),
                                target: target.clone(),
                            },
                        ))),

                        (Opcode::Copy(_), _) => Err(InvalidCommand::UnknownParameterSet(value)),*/
                        $(
                            $(
                                (Opcode::[<$cmd:camel>](_), [$(
                                    Ope {parameter: $param_type($param_name), following_colon: _ }
                                ),*]) => Ok(Command::[<$cmd:camel>]([<$cmd:camel>]::[<$variant_name:camel>](
                                    [<$variant_name:camel>] {
                                        opcode: opcode.clone(),
                                        $(
                                            $param_name : $param_name.clone(),
                                        )*
                                    }
                                ))),
                            )+
                            (Opcode::[<$cmd:camel>](_), _ ) => Err(InvalidCommand::UnknownParameterSet(value)),
                        )+
                    }
                }
            }

        }
    };
}

fn is_opcode_char(c: char) -> bool {
    c == '_' || AsChar::is_alphanum(c)
}

impl<'a> FromSpan<'a> for Opcode<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, opcode_span) = take_while1(is_opcode_char)(s)?;
        let (s, _) = peek(alt((multispace1, eof)))(s)?;
        let opcode = Opcode::try_from(opcode_span)?;
        Ok((s, opcode))
    }
}

impl<'a> FromSpan<'a> for Command<'a> {
    fn parse_span<
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'a str>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'a str>>,
    >(
        s: Span<'a>,
    ) -> nom::IResult<Span, Self, E> {
        let (s, operation) = Operation::parse_span(s)?;
        Ok((s, Command::try_from(operation)?))
    }
}

commands! {
    // alot of these names are from https://github.com/Backseating-Committee-2k/BackseatSafeSystem2k/blob/e7cd8ea5ed4bbd5ee8778c98bffbdfa9ac6d12c1/src/opcodes.rs
    // hence the naming is identical to that
    // todo: figure out if e.g. MoveTargetPointer is specified as copy <Pointer> <register> even
    // if the name suggests specifiing the target first
    copy VARIANTS {
        MoveRegisterImmediate {value: Value, target: Register} "move the value into the register",
        MoveRegisterAddress {source: Address, target: Register} "move the value at address source into the register",
        MoveTargetSource {source: Register, target: Register} "move the contents of register source to register target",
        MoveAddressRegister {source: Register, target: Address} "move the contents of register source into memory at address target",
        MoveTargetPointer {source: PointerRegister, target: Register} "Move the contents addressed by source into register target",
        MovePointerSource {source: Address, target: PointerRegister} "Move the contents addressed by the value of sourceinto memory ad address specified by register target",
    },

    copy_byte VARIANTS {
        MoveByteRegisterAddress {source: Address, target: Register} "move the value at address source into register target (1 byte)",
        MoveByteAddressRegister {source: Register, target: Address} "move the contents of register source into memory at address target (1 byte)",
        MoveByteTargetPointer {source: PointerRegister, target: Register } "move the contents addressed by the value of register source into register target (1 byte)",
        MoveBytePointerSource {source: Register, target: PointerRegister} "move the contents of register source to the memory at the address specified by target (1 byte)",
    },
    copy_halfword VARIANTS {
        MoveHalfwordRegisterAddress {source: Address, target: Register} "move the value at address source into register target (2 bytes)",
        MoveHalfwordAddressRegister {source: Register, target: Address} "move the contents of register source into memory at address target (2 bytes)",
        MoveHalfwordTargetPointer {source: PointerRegister, target: Register } "move the contents addressed by the value of register source into register target (2 bytes)",
        MoveHalfwordPointerSource {source: Register, target: PointerRegister} "move the contents of register source to the memory at the address specified by target (2 bytes)",
    },

    offset_copy VARIANTS {
        MovePointerSourceOffset {source: Register, offset: Value, target: PointerRegister } "move the value of register source into memory at address pointer + offset",
        MoveTargetPointerOffset {source: PointerRegister, offset: Value, target: Register } "move the value of the memory at register source offset by the offset value into register target",
    },
    offset_copy_byte VARIANTS {
        MoveByteTargetPointerOffset {source: PointerRegister, offset: Value, target: Register } "move the value of the memory at register source offset by the offset value into register target (1 byte)",
        MoveBytePointerSourceOffset {source: Register, offset: Value, target: PointerRegister } "move the value of register source into memory at address pointer + offset (1 byte)",
    },
    offset_copy_halfword VARIANTS {
        MoveHalfwordPointerSourceOffset {source: Register, offset: Value, target: PointerRegister } "move the value of register source into memory at address pointer + offset (2 bytes)",
        MoveHalfwordTargetPointerOffset {source: PointerRegister, offset: Value, target: Register } "move the value of the memory at register source offset by the offset value into register target (2 bytes)",
    },

    halt VARIANTS {
        HaltAndCatchFire{} "halt and catch fire",
    },

    add VARIANTS {
        AddTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "add the values in registers lhs and rhs, store the result in target, set zero and carry flags appropriately",
        AddTargetSourceImmediate {lhs: Register, rhs: Value, target: Register } "add the value in register lhs and value rhs, store the result in target, set zero and carry flags appropriately",
    },
    add_carry VARIANTS {
        AddWithCarryTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "add (with carry) the values in registers lhs and rhs, store the result in target, set zero and carry flags appropriately",
    },
    sub VARIANTS {
        SubstractTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "substract the values in registers lhs and rhs, store the result in target, set zero and carry flags appropriately",
        SubstractTargetSourceImmediate {lhs: Register, rhs: Value, target: Register } "substract the value in register lhs and value rhs, store the result in target, set zero and carry flags appropriately",
    },
    sub_carry VARIANTS {
        SubtractWithCarryTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "substract (with carry) the values in registers lhs and rhs, store the result in target, set zero and carry flags appropriately",
    },
    mult VARIANTS {
        MultiplyHighLowLhsRhs {lhs: Register, rhs: Register, target_low: Register, target_high: Register } "multiply the values in registers lhs and rhs, store the result in target_low and target_high, set zero and carry flags appropriately",
    },
    divmod VARIANTS {
        DivmodTargetModLhsRhs {lhs: Register, rhs: Register, target_div: Register, target_mod: Register } "divmod the values in registers lhs and rhs, store the result in target_div and the remainder in target_mod, set zero and divide-by-zero flags appropriately",
    },

    and VARIANTS {
        AndTargetLhsRhs {lhs: Register, rhs: Register, target: Register}  "bitwise and the values in registers lhs and rhs, store the result in target, set zero flag appropriately",
    },
    or VARIANTS {
        OrTargetLhsRhs {lhs: Register, rhs: Register, target: Register}  "bitwise or the values in registers lhs and rhs, store the result in target, set zero flag appropriately",
    },
    xor VARIANTS {
        XorTargetLhsRhs {lhs: Register, rhs: Register, target: Register}  "bitwise xor the values in registers lhs and rhs, store the result in target, set zero flag appropriately",
    },
    not VARIANTS{
        NotTargetSource {source: Register, target: Register} "bitwise not the value in register source, store the result in target, set zero flag appropriately",
    },
    lshift VARIANTS {
        LeftShiftTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "left shift the value in register lhs by rhs bits, store the result in target, set zero and carry flags appropriately",
    },
    rshift VARIANTS {
        RightShiftTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "right shift the value in register lhs by rhs bits, store the result in target, set zero and carry flags appropriately",
    },

    comp VARIANTS {
        CompareTargetLhsRhs {lhs: Register, rhs: Register, target: Register } "compare the values in registers lhs and rhs, store the result (Word::MAX, 0, 1) in target, set zero flag appropriately",
    },
    comp_eq VARIANTS {
        BoolCompareEquals {lhs: Register, rhs: Register, target: Register } "checks whether the values in registers Lhs and Rhs are equal and stores the result as boolean (0 or 1) in register target",
    },
    comp_neq VARIANTS {
        BoolCompareNotEquals {lhs: Register, rhs: Register, target: Register } "checks whether the values in registers Lhs and Rhs are not equal and stores the result as boolean (0 or 1) in register target",
    },
    comp_gt VARIANTS {
        BoolCompareGreater {lhs: Register, rhs: Register, target: Register } "checks whether the values in register Lhs is greater than the value in register Rhs and stores the result as boolean (0 or 1) in register target",
    },
    comp_ge VARIANTS {
        BoolCompareGreaterOrEquals {lhs: Register, rhs: Register, target: Register } "checks whether the values in register Lhs is greater than or equal to the value in register Rhs and stores the result as boolean (0 or 1) in register target",
    },
    comp_lt VARIANTS {
        BoolCompareLess {lhs: Register, rhs: Register, target: Register } "checks whether the values in register Lhs is less than the value in register Rhs and stores the result as boolean (0 or 1) in register target",
    },
    comp_le VARIANTS {
        BoolCompareLessOrEquals {lhs: Register, rhs: Register, target: Register } "checks whether the values in register Lhs is less than or equal to the value in register Rhs and stores the result as boolean (0 or 1) in register target",
    },

    push VARIANTS {
        PushRegister {source: Register} "push the contents of register source onto the stack",
        PushImmediate {value: Value} "pushes the value value onto the stack",
    },
    pop VARIANTS {
        PopRegister{target: Register} "pops from the stack and stores the value in register target",
        PopOp{} "pops from the stack and discards the value",
    },
    call VARIANTS {
        CallImmediate{callee: Address} "push the current instruction pointer onto the stack and jump to the specified address",
        CallRegister{callee: Register} "push the current instruction pointer onto the stack and jump to the specified by the address in register callee",
        CallRegisterPointer{callee: PointerRegister} "push the current instruction pointer onto the stack and jump to the specified address in memory pointed to by register callee",
    },
    return VARIANTS {
        ReturnOp {} "pop the return address from the stack and jump to it",
    },
    jump VARIANTS {
        JumpImmediate{target: Address} "jump to the specified address",
        JumpRegister{target: Register} "jump to the specified by the address in register target",
    },
    jump_eq VARIANTS {
        JumpImmediateIfEqual{compare_result: Register, target: Address} "Jump to the specified address if the comparison result in [compare_result] is \"equality\"",
        JumpRegisterIfEqual{compare_result: Register, target: Register} "Jump to the address specified by the register target if the comparison result in [compare_result] is \"equality\"",
    },
    jump_gt VARIANTS {
        JumpImmediateIfGreaterThan{compare_result: Register, target: Address} "Jump to the specified address if the comparison result in [compare_result] is \"greater than\"",
        JumpRegisterIfGreaterThan{compare_result: Register, target: Register} "Jump to the address specified by the register target if the comparison result in [compare_result] is \"greater than\"",
    },
    jump_lt VARIANTS {
        JumpImmediateIfLessThan{compare_result: Register, target: Address} "Jump to the specified address if the comparison result in [compare_result] is \"less than\"",
        JumpRegisterIfLessThan{compare_result: Register, target: Register} "Jump to the address specified by the register target if the comparison result in [compare_result] is \"less than\"",
    },
    jump_gteq VARIANTS {
        JumpImmediateIfGreaterThanOrEqual{compare_result: Register, target: Address} "Jump to the specified address if the comparison result in [compare_result] is \"greater than\" or \"equal\"",
        JumpRegisterIfGreaterThanOrEqual{compare_result: Register, target: Register} "Jump to the address specified by the register target if the comparison result in [compare_result] is \"greater than\" or \"equal\"",
    },
    jump_lteq VARIANTS {
        JumpImmediateIfLessThanOrEqual{compare_result: Register, target: Address} "Jump to the specified address if the comparison result in [compare_result] is \"less than\" or \"equal\"",
        JumpRegisterIfLessThanOrEqual{compare_result: Register, target: Register} "Jump to the address specified by the register target if the comparison result in [compare_result] is \"less than\" or \"equal\"",
    },
    jump_zero VARIANTS {
        JumpImmediateIfZero{target: Address} "Jump to the specified address if the zero flag is set",
        JumpRegisterIfZero{target: Register} "Jump to the address specified by the register target if the zero flag is set",
    },
    jump_not_zero VARIANTS {
        JumpImmediateIfNotZero{target: Value} "Jump to the specified address if the zero flag is not set",
        JumpRegisterIfNotZero{target: Register} "Jump to the address specified by the register target if the zero flag is not set",
    },
    jump_carry VARIANTS {
        JumpImmediateIfCarry{target: Address} "Jump to the specified address if the carry flag is set",
        JumpRegisterIfCarry{target: Register} "Jump to the address specified by the register target if the carry flag is set",
    },
    jump_not_carry VARIANTS {
        JumpImmediateIfNotCarry{target: Address} "Jump to the specified address if the carry flag is not set",
        JumpRegisterIfNotCarry{target: Register} "Jump to the address specified by the register target if the carry flag is not set",
    },
    jump_divide_by_zero VARIANTS {
        JumpImmediateIfDivideByZero{target: Address} "Jump to the specified address if the divide-by-zero flag is set",
        JumpRegisterIfDivideByZero{target: Register} "Jump to the address specified by the register target if the divide-by-zero flag is set",
    },
    jump_not_divide_by_zero VARIANTS {
        JumpImmediateIfNotDivideByZero{target: Address} "Jump to the specified address if the divide-by-zero flag is not set",
        JumpRegisterIfNotDivideByZero{target: Register} "Jump to the address specified by the register target if the divide-by-zero flag is not set",
    },

    noop VARIANTS {
        NoOpOp{} "does nothing",
    },

    get_key VARIANTS {
        GetKeyState {keycode: Register, target: Register} "store the keystate (1 = held down, 0 = not held down) of the key specified by register [keycode] into register [target] and set the zero flag appropriately",
    },
    poll_time VARIANTS {
        PollTimeOp{target_low: Register, target_high: Register} "store the number of milliseconds since the UNIX epoch into registers [target_high] and [target_low]",
    },

    swap VARIANTS {
        SwapFramebuffers{} "swap the display buffers",
    },
    draw_buffer_addr VARIANTS {
        InvisibleFramebufferAddress {target: Register} "get the start address of the framebuffer that's currently invisible (use the address to draw without tearing)",
    },


    poll_cycles VARIANTS {
        PollCycleCountHighLow{target_high: Register, target_low: Register }"store the current cycle (64 bit value) count into registers [target_high] and [target_low]",
    },
    dump_registers VARIANTS {
        DumpRegistersOp{} "dump the contents of all registers into the file 'registers_YYYY-MM-DD_X.bin' where YYYY-MM-DD is the current date and X is an increasing number",
    },
    dump_memory VARIANTS {
        DumpMemoryOp{} "dump the contents of the whole memory into the file 'memory_YYYY-MM-DD_X.bin' where YYYY-MM-DD is the current date and X is an increasing number",
    },

    assert VARIANTS {
        AssertRegisterRegister{actual: Register, expected: Register} "assert that the expected register value equals the actual register value (behavior of the VM on a failed assertion is implementation defined)",
        AssertRegisterImmediate{actual: Register, expected: Value} "assert that the expected value equals the actual register value (behavior of the VM on a failed assertion is implementation defined)",
        AssertPointerImmediate{actual: PointerRegister, expected: Value} "assert that the value in memory at the location pointed to by actual equals the actual register value (behavior of the VM on a failed assertion is implementation defined)",
    },
    debug_break VARIANTS {
        DebugBreakOp{} "behavior is implementation defined",
    },
    print VARIANTS {
        PrintRegister {source: Register} "prints the value of the register as debug output",
    },
    checkpoint VARIANTS {
        CheckpointOp{value: Value} "makes the emulator check the value of the internal checkpoint counter, fails on mismatch",
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{combinator::all_consuming, error::VerboseError};
    use rstest::rstest;

    #[rstest]
    #[case::add_register_to_value("add R0, 1, R2")]
    #[case::copy_byte_ptr_register("COPY_BYTE *R1, R1")]
    #[case::jump_lt("jump_lt r5, jump_target")]
    #[case::noop("noop")]
    fn test_commands(#[case] input: &str) {
        let span = nom_locate::LocatedSpan::new(input);
        let (_, operation) =
            all_consuming::<_, _, VerboseError<_>, _>(Operation::parse_span)(span).unwrap();
        let _command = Command::try_from(operation).unwrap();
    }
}
