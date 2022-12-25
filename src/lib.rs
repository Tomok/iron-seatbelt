pub mod parser_combinator {
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_until, take_while1},
        character::complete::{anychar, digit1, hex_digit1, multispace0, multispace1, one_of},
        combinator::{all_consuming, map, opt},
        error::{context, ContextError, Error, ErrorKind, ParseError, VerboseError},
        multi::{many0, many1},
        sequence::{pair, preceded, separated_pair, terminated, tuple},
        AsChar, IResult,
    };
    use nom_locate::LocatedSpan;

    use nom::character::complete::char as char_tag; //redefine to avoid confusion with char type

    type Span<'a> = LocatedSpan<&'a str>;

    #[derive(PartialEq, Eq, Debug)]
    pub struct BackSeatFile<'s> {
        entries: Vec<FileEntry<'s>>,
    }

    impl<'s> BackSeatFile<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, (entries, _whitespace)) = context(
                "File",
                all_consuming(tuple((many0(FileEntry::parse_span), multispace0))),
            )(s)?;
            Ok((s, Self { entries }))
        }
    }

    impl<'s> BackSeatFile<'s> {
        pub fn entries(&self) -> &[FileEntry] {
            self.entries.as_ref()
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum FileEntry<'s> {
        Import(Import<'s>),
        Function(Function<'s>),
    }

    impl<'s> FileEntry<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, res) = alt((
                map(Import::parse_span, Self::Import),
                map(Function::parse_span, Self::Function),
            ))(s)?;
            dbg!((&s, &res));
            Ok((s, res))
        }

        /// Returns `true` if the file entry is [`Import`].
        ///
        /// [`Import`]: FileEntry::Import
        #[must_use]
        pub fn is_import(&self) -> bool {
            matches!(self, Self::Import(..))
        }

        #[must_use]
        pub fn as_import(&self) -> Option<&Import<'s>> {
            if let Self::Import(v) = self {
                Some(v)
            } else {
                None
            }
        }

        /// Returns `true` if the file entry is [`Function`].
        ///
        /// [`Function`]: FileEntry::Function
        #[must_use]
        pub fn is_function(&self) -> bool {
            matches!(self, Self::Function(..))
        }

        #[must_use]
        pub fn as_function(&self) -> Option<&Function<'s>> {
            if let Self::Function(v) = self {
                Some(v)
            } else {
                None
            }
        }
    }
    #[derive(PartialEq, Eq, Debug)]
    pub struct Import<'s> {
        import: Span<'s>,
        segments: Vec<ImportSegment<'s>>,
        semicomoln: Span<'s>,
    }

    impl<'s> Import<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, _) = multispace0(s)?;
            let (s, import) = tag("import")(s)?;
            let (s, _) = multispace1(s)?;
            let (s, segments) = many1(ImportSegment::parse_span)(s)?; //todo: check correct usage
                                                                      //of dots in import
                                                                      //statements
            let (s, _) = multispace0(s)?;
            let (s, semicomoln) = tag(";")(s)?;
            Ok((
                s,
                Self {
                    import,
                    segments,
                    semicomoln,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct ImportSegment<'s> {
        ident: Ident<'s>,
        dot: Option<Span<'s>>,
    }

    impl<'s> ImportSegment<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, ident) = Ident::parse_span(s)?;
            let (s, dot) = opt(tag("."))(s)?;
            Ok((s, Self { ident, dot }))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct Function<'s> {
        function: Span<'s>,
        name: Ident<'s>,
        parameters: Parameters<'s>,
        arrow: Span<'s>,
        return_type: Ident<'s>,
        code_block: CodeBlock<'s>,
    }

    impl<'s> Function<'s> {
        pub fn function(&self) -> LocatedSpan<&str, ()> {
            self.function
        }

        pub fn name(&self) -> &Ident<'s> {
            &self.name
        }

        pub fn arrow(&self) -> LocatedSpan<&str, ()> {
            self.arrow
        }

        pub fn code_block(&self) -> &CodeBlock<'s> {
            &self.code_block
        }

        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, _) = multispace0(s)?;
            let (s, function) = tag("function")(s)?;
            let (s, _) = multispace1(s)?;
            let (s, name) = Ident::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, parameters) = Parameters::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, arrow) = tag("~>")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, return_type) = Ident::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, code_block) = CodeBlock::parse_span(s)?;
            Ok((
                s,
                Self {
                    function,
                    name,
                    parameters,
                    arrow,
                    return_type,
                    code_block,
                },
            ))
        }

        pub fn return_type(&self) -> &Ident<'s> {
            &self.return_type
        }

        pub fn parameters(&self) -> &Parameters<'s> {
            &self.parameters
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct Parameters<'s> {
        open_brace: Span<'s>,
        params: Vec<Parameter<'s>>, //should the `,` be stored as well, if included??
        closing_brace: Span<'s>,
    }

    impl<'s> Parameters<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, open_brace) = tag("(")(s)?;
            let mut params = Vec::new();
            let mut next_param_allowed = true; //checks if a parameter is allowed, i.e. it is the
                                               //first parameter or the last one was followed by a
                                               //`,`
            let mut read_pos = s; //used to keep position after each loop
            let closing_brace = loop {
                let (s, _) = multispace0(read_pos)?;
                //is end of parameter list?
                match tag(")")(s) {
                    Ok((s, closing_brace)) => {
                        //is end of parameter list
                        read_pos = s;
                        break closing_brace;
                    }
                    Err(nom::Err::Error(Error {
                        input,
                        code: ErrorKind::Tag,
                    })) => {
                        //is parameter?
                        if !next_param_allowed {
                            dbg!("No more paramenters allowed");
                            //todo: how to state that a `,` or a `)` were expected?
                            //return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)));
                            todo!("crashing ... need to figure out how to raise a usefull parameter instead");
                        }
                        let (s, param) = Parameter::parse_span(input)?;
                        next_param_allowed = param.comma.is_some();
                        params.push(param);
                        let (s, _) = multispace0(s)?;
                        read_pos = s;
                    }
                    Err(e) => {
                        todo!("crashing ... need to figure out how to raise a usefull parameter instead");
                        //return Err(e); //raise every other error
                    }
                }
            };
            let s = read_pos;
            Ok((
                s,
                Self {
                    open_brace,
                    params,
                    closing_brace,
                },
            ))
        }

        pub fn params(&self) -> &[Parameter] {
            self.params.as_ref()
        }
    }
    #[derive(PartialEq, Eq, Debug)]
    pub struct Parameter<'s> {
        name: Ident<'s>,
        colon: Span<'s>,
        typ: Ident<'s>,
        /// the `,` following the parameter, if it exists
        comma: Option<Span<'s>>,
    }

    impl<'s> Parameter<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, name) = Ident::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, colon) = tag(":")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, typ) = Ident::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, comma) = opt(tag(","))(s)?;
            let (s, _) = multispace0(s)?;
            Ok((
                s,
                Self {
                    name,
                    colon,
                    typ,
                    comma,
                },
            ))
        }

        pub fn name(&self) -> &Ident<'s> {
            &self.name
        }

        pub fn typ(&self) -> &Ident<'s> {
            &self.typ
        }
    }

    /// a code block surrounded by `{}`
    #[derive(PartialEq, Eq, Debug)]
    pub struct CodeBlock<'s> {
        open_curly_brace: Span<'s>,
        //todo
        closed_curly_brace: Span<'s>,
    }

    impl<'s> CodeBlock<'s> {
        pub fn open_curly_brace(&self) -> LocatedSpan<&str, ()> {
            self.open_curly_brace
        }

        pub fn closed_curly_brace(&self) -> LocatedSpan<&str, ()> {
            self.closed_curly_brace
        }

        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, open_curly_brace) = tag("{")(s)?;
            let (s, _) = multispace0(s)?;
            //todo replace with actual parsing
            let (s, _) = take_until("}")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, closed_curly_brace) = tag("}")(s)?;
            Ok((
                s,
                Self {
                    open_curly_brace,
                    closed_curly_brace,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum Statement<'s> {
        LetStatement(LetStatement<'s>),
        IfStatement(IfStatement<'s>),
        //ForLoop(ForLoop<'s>),
        CodeBlock(CodeBlock<'s>),
        Expression(Expression<'s>),
        //InlineBssembly(InlineBssembly<'s>),
    }

    impl<'s> Statement<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(LetStatement::parse_span, Self::LetStatement),
                map(IfStatement::parse_span, Self::IfStatement),
                //todo map(ForLoop::parse_span, Self::ForLoop),
                map(CodeBlock::parse_span, Self::CodeBlock),
                map(Expression::parse_span, Self::Expression),
                //todo map(InlineBssembly::parse_span, Self::InlineBssembly),
            ))(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct LetStatement<'s> {
        let_token: Span<'s>,
        variable_name: Ident<'s>,
        colon: Span<'s>,
        mutable: Option<Span<'s>>,
        typ: Ident<'s>,
        equals: Span<'s>,
        assignment: Expression<'s>,
    }

    impl<'s> LetStatement<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, let_token) = tag("let")(s)?;
            let (s, _) = multispace1(s)?;
            let (s, variable_name) = Ident::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, colon) = tag(":")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, mutable) = opt(tag("mutable"))(s)?;
            let (s, _) = multispace0(s)?;
            let (s, typ) = Ident::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, equals) = tag("=")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, assignment) = Expression::parse_span(s)?;
            Ok((
                s,
                Self {
                    let_token,
                    variable_name,
                    colon,
                    mutable,
                    typ,
                    equals,
                    assignment,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct IfStatement<'s> {
        if_token: Span<'s>,
        condition: Expression<'s>,
        then_block: CodeBlock<'s>,
        // todo is else mandatory??
        else_token: Span<'s>,
        else_type: ElseType<'s>,
    }

    impl<'s> IfStatement<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, if_token) = tag("if")(s)?;
            let (s, _) = multispace1(s)?;
            let (s, condition) = Expression::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, then_block) = CodeBlock::parse_span(s)?;
            let (s, _) = multispace0(s)?;
            let (s, else_token) = tag("else")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, else_type) = ElseType::parse_span(s)?;
            Ok((
                s,
                Self {
                    if_token,
                    condition,
                    then_block,
                    else_token,
                    else_type,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum ElseType<'s> {
        /// just a CodeBlock (`{...}`)
        Normal(CodeBlock<'s>),
        /// an else if statement
        ElseIf(Box<IfStatement<'s>>),
    }

    impl<'s> ElseType<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(CodeBlock::parse_span, Self::Normal),
                map(
                    preceded(
                        multispace1, //there needs to be a whitespace between `else` and the
                        //following `if`
                        IfStatement::parse_span,
                    ),
                    |stmt| Self::ElseIf(Box::new(stmt)),
                ),
            ))(s)
        }
    }

    /*#[derive(PartialEq, Eq, Debug)]
    pub struct ForLoop<'s> {
        //todo
    }*/

    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression<'s> {
        IntLiteral(IntLiteral<'s>),
        CharLiteral(CharLiteral<'s>),
        /// an Ident, could be a variable_name or a function name
        Ident(Ident<'s>),
        FunctionCall(FunctionCall<'s>),
    }

    impl<'s> Expression<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(IntLiteral::parse_span, Self::IntLiteral),
                map(CharLiteral::parse_span, Self::CharLiteral),
                map(FunctionCall::parse_span, Self::FunctionCall),
                map(Ident::parse_span, Self::Ident),
            ))(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct FunctionCall<'s> {
        function_expr: Box<Expression<'s>>, //does this make sense? is there a more specific type,
        //that could be used?
        open_brace: Span<'s>,
        parameters: Vec<FunctionCallParameter<'s>>,
        closing_brace: Span<'s>,
    }

    impl<'s> FunctionCall<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, expr) = Expression::parse_span(s)?;
            let function_expr = Box::new(expr);
            let (s, open_brace) = tag("(")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, parameters) = many0(FunctionCallParameter::parse_span)(s)?;
            let (s, closing_brace) = tag(")")(s)?;
            let (s, _) = multispace0(s)?;
            Ok((
                s,
                Self {
                    function_expr,
                    open_brace,
                    parameters,
                    closing_brace,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct FunctionCallParameter<'s> {
        expression: Box<Expression<'s>>,
        comma: Option<Span<'s>>,
    }

    impl<'s> FunctionCallParameter<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            terminated(
                map(
                    separated_pair(Expression::parse_span, multispace0, opt(tag(","))),
                    |(expr, comma)| Self {
                        expression: Box::new(expr),
                        comma,
                    },
                ),
                multispace0,
            )(s)
        }
    }

    /*#[derive(PartialEq, Eq, Debug)]
    pub struct InlineBssembly<'s> {
        //todo
    }*/

    #[derive(PartialEq, Eq, Debug)]
    pub enum IntLiteral<'s> {
        ///no identifier -> Value is decimal
        Dec {
            digit_segments: Vec<DigitSegment<'s>>,
        },
        /// `0x` -> value is hexadecimal
        Hex {
            base_identifier: Span<'s>,
            digit_segments: Vec<DigitSegment<'s>>,
        },
        /// `0xb` -> value is binary
        Bin {
            base_identifier: Span<'s>,
            digit_segments: Vec<DigitSegment<'s>>,
        },
        /// `0o` -> value is octal
        Oct {
            base_identifier: Span<'s>,
            digit_segments: Vec<DigitSegment<'s>>,
        },
    }

    impl<'s> IntLiteral<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(
                    pair(tag("0x"), many1(DigitSegment::parse_span_hex)),
                    |(base_identifier, digit_segments)| Self::Hex {
                        base_identifier,
                        digit_segments,
                    },
                ),
                map(
                    pair(tag("0b"), many1(DigitSegment::parse_span_bin)),
                    |(base_identifier, digit_segments)| Self::Bin {
                        base_identifier,
                        digit_segments,
                    },
                ),
                map(
                    pair(tag("0o"), many1(DigitSegment::parse_span_oct)),
                    |(base_identifier, digit_segments)| Self::Oct {
                        base_identifier,
                        digit_segments,
                    },
                ),
                map(many1(DigitSegment::parse_span_dec), |digit_segments| {
                    Self::Dec { digit_segments }
                }),
            ))(s)
        }

        pub fn base_identifier(&self) -> Option<&Span<'s>> {
            match self {
                IntLiteral::Dec { .. } => None,
                IntLiteral::Hex {
                    base_identifier, ..
                }
                | IntLiteral::Bin {
                    base_identifier, ..
                }
                | IntLiteral::Oct {
                    base_identifier, ..
                } => Some(base_identifier),
            }
        }

        pub fn digit_segments(&self) -> &[DigitSegment] {
            match self {
                IntLiteral::Dec { digit_segments }
                | IntLiteral::Hex {
                    base_identifier: _,
                    digit_segments,
                }
                | IntLiteral::Bin {
                    base_identifier: _,
                    digit_segments,
                }
                | IntLiteral::Oct {
                    base_identifier: _,
                    digit_segments,
                } => digit_segments,
            }
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct DigitSegment<'s> {
        digits: Span<'s>,
        underscore: Option<Span<'s>>,
    }

    impl<'s> DigitSegment<'s> {
        pub fn parse_span_hex<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            map(pair(hex_digit1, opt(tag("_"))), |(digits, underscore)| {
                Self { digits, underscore }
            })(s)
        }

        pub fn parse_span_bin<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            fn is_bin(c: char) -> bool {
                "01".contains(c)
            }
            map(
                pair(take_while1(is_bin), opt(tag("_"))),
                |(digits, underscore)| Self { digits, underscore },
            )(s)
        }

        pub fn parse_span_oct<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            fn is_bin(c: char) -> bool {
                "01234567".contains(c)
            }
            map(
                pair(take_while1(is_bin), opt(tag("_"))),
                |(digits, underscore)| Self { digits, underscore },
            )(s)
        }

        pub fn parse_span_dec<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            map(pair(digit1, opt(tag("_"))), |(digits, underscore)| Self {
                digits,
                underscore,
            })(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct CharLiteral<'s> {
        opening_apostrophe: Span<'s>,
        character_definition: SingleCharacterDefinition,
        closing_apostrophe: Span<'s>,
    }

    impl<'s> CharLiteral<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            map(
                tuple((tag("'"), SingleCharacterDefinition::parse_span, tag("'"))),
                |(opening_apostrophe, character_definition, closing_apostrophe)| Self {
                    opening_apostrophe,
                    character_definition,
                    closing_apostrophe,
                },
            )(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum SingleCharacterDefinition {
        /// a single, not escaped character
        SimpleChar(char),
        /// a character prefixed with a `\` to be converted into the correct character
        EscapedChar(char),
    }

    impl SingleCharacterDefinition {
        pub fn parse_span<
            's,
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(
                    //todo: convert one_of parameters into constant
                    preceded(char_tag('\\'), one_of("t\\nvfr0")),
                    Self::EscapedChar,
                ),
                map(anychar, Self::SimpleChar),
            ))(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct Ident<'s>(Span<'s>);
    impl<'s> Ident<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, ident) = take_while1(AsChar::is_alphanum)(s)?;
            //todo: check that ident does not start with a number
            Ok((s, Self(ident)))
        }
    }

    impl<'s> From<&'s Ident<'s>> for &'s str {
        fn from(value: &'s Ident<'s>) -> Self {
            value.0.fragment()
        }
    }

    pub fn parse(input: &str) -> Result<BackSeatFile, ()> {
        //todo: error type
        let input = LocatedSpan::new(input);
        match BackSeatFile::parse_span::<VerboseError<LocatedSpan<&str>>>(input) {
            Ok((_, bs)) => Ok(bs),
            Err(e) => {
                todo!();
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use std::fs::read_to_string;

        use super::*;
        use nom::error::VerboseError;
        use textwrap::dedent;

        use rstest::*;

        #[test]
        fn test_bool_type() {
            let input = dedent(
                "
                function main() ~> Nothing {
                    let b: Bool = true;
                    let c: Bool = false;
                }",
            );
            let bs_file = parse(&input).unwrap();
            assert_eq!(1, bs_file.entries().len());
            let bs_function = bs_file.entries()[0].as_function().unwrap();
            assert_eq!("main", <&str>::from(bs_function.name()));
            assert_eq!("Nothing", <&str>::from(bs_function.return_type()));
        }

        #[test]
        fn test_function_parameters() {
            let input = dedent(
                "
                function f(a: U32, b: U32) ~> Nothing {
                }",
            );
            let (_, bs_file) =
                BackSeatFile::parse_span::<VerboseError<_>>(LocatedSpan::new(&input)).unwrap(); //parse(&input).unwrap();
            assert_eq!(1, bs_file.entries().len());
            let bs_function = bs_file.entries()[0].as_function().unwrap();
            assert_eq!("f", <&str>::from(bs_function.name()));
            assert_eq!("Nothing", <&str>::from(bs_function.return_type()));
            let params = bs_function.parameters().params();
            assert_eq!(2, params.len());
            let param0 = &params[0];
            assert_eq!("a", <&str>::from(param0.name()));
            assert_eq!("U32", <&str>::from(param0.typ()));
            let param1 = &params[1];
            assert_eq!("b", <&str>::from(param1.name()));
            assert_eq!("U32", <&str>::from(param1.typ()));
        }

        #[rstest]
        #[case("undeclared_types/test_use_of_undeclared_parameter_type_fails.bs")]
        #[case("undeclared_types/test_use_of_undeclared_return_type_fails.bs")]
        #[case("test_if.bs")]
        #[case("test_import.bs")]
        #[case("test_namespace_with_multiple_definitions.bs")]
        #[case("assignments/test_assignment_to_const_fails.bs")]
        #[case("assignments/test_assignment.bs")]
        #[case("test_empty_main_function.bs")]
        #[case("test_bool_type.bs")]
        #[case("test_modulo.bs")]
        #[case("subfolder/file.bs")]
        #[case("subfolder/relative_to_import.bs")]
        #[case("test_name_lookup_in_block.bs")]
        #[case("test_type_deduction.bs")]
        #[case("test_no_implicit_const_cast.bs")]
        #[case("test_order_of_evaluation_in_nested_call.bs")]
        #[case("pointers/test_dereferencing_bool_pointer.bs")]
        #[case("pointers/test_pointer_usage.bs")]
        #[case("pointers/test_cannot_subtract_pointer_from_integer.bs")]
        #[case("pointers/test_pointer_arithmetics.bs")]
        #[case("pointers/test_assigning_const_pointer_to_mutable_pointer_fails.bs")]
        #[case("import_test_file.bs")]
        #[case("disable_test_regular_nothing.bs")]
        #[case("structs/test_arrays_of_structs.bs")]
        #[case("structs/test_structs_inside_other_structs.bs")]
        #[case("structs/test_structs_in_namespaces.bs")]
        #[case("structs/test_pointer_arithmetics_with_arrays_of_structs.bs")]
        #[case("test_loop_break_continue.bs")]
        #[case("asserts/test_assert_equals_fails.bs")]
        #[case("asserts/test_assert_fails.bs")]
        #[case("asserts/test_assert_succeeds.bs")]
        #[case("asserts/test_assert_equals_succeeds.bs")]
        #[case("test_if_else.bs")]
        #[case("char_literals/test_single_backslash_as_char_fails.bs")]
        #[case("char_literals/test_invalid_escape_sequence.bs")]
        #[case("char_literals/test_valid_char_literals.bs")]
        #[case("test_char.bs")]
        #[case("test_const_mutable.bs")]
        #[case("nothing/test_compare_nothing_literals.bs")]
        #[case("nothing/test_nothing_as_stack_variable.bs")]
        #[case("nothing/test_initializing_nothing_variable_with_function_call.bs")]
        #[case("nothing/test_nothing_as_function_parameter.bs")]
        #[case("test_shadowing.bs")]
        #[case("test_size_of_expressions_and_types.bs")]
        #[case("test_while.bs")]
        #[case("test_u32_equality.bs")]
        #[case("test_bool_operations.bs")]
        #[case("test_relational_operators.bs")]
        #[case("test_boolean_not.bs")]
        #[case("functions/test_no_main_function_fails.bs")]
        #[case("functions/test_implicit_return_type.bs")]
        #[case("functions/test_return_statement.bs")]
        #[case("functions/test_function_calls.bs")]
        #[case("functions/test_return_result_of_function_call_with_return_type_nothing.bs")]
        #[case("functions/test_exporting_global_function_fails.bs")]
        #[case("functions/test_calling_non_exported_function_from_outside_namespace_fails.bs")]
        #[case("test_do_while.bs")]
        #[case("test_hello_world.bs")]
        #[case("test_inline_bssembly_with_string.bs")]
        #[case("test_if_variable_in_block.bs")]
        #[case("test_for.bs")]
        #[case("integer_literals/test_integer_literal_binary_too_big.bs")]
        #[case("integer_literals/test_integer_literal_hexadecimal_too_big.bs")]
        #[case("integer_literals/test_integer_literal_decimal_too_big.bs")]
        #[case("integer_literals/test_integer_literal_octal_too_big.bs")]
        #[case("integer_literals/test_integer_literals_with_underscores.bs")]
        #[case("integer_literals/test_integer_literals_max_values.bs")]
        #[case("test_assignment_expression.bs")]
        #[case("arrays/test_array_pass_by_value.bs")]
        #[case("arrays/test_array_as_return_value.bs")]
        #[case("arrays/test_simple_array.bs")]
        #[case("arrays/test_array_of_nothing.bs")]
        #[case("arrays/test_array_of_function_pointers.bs")]
        #[case("arrays/test_two_dimensional_array.bs")]
        #[trace]
        /// tests parsing the files, does not check if the parsed result is correct
        fn test_parse_tests_from_original_seatbelt_repo(#[case] testfile: &str) {
            let filename = format!("./res/tests/from_seatbelt/{}", testfile);
            let script = read_to_string(filename).unwrap();
            let input = LocatedSpan::new(script.as_str());
            let result = BackSeatFile::parse_span::<VerboseError<_>>(input);
            if let Err(e) = result {
                //this should not happen, but let's generate a nice error message:
                panic!("Parsing failed - error: {:#?}", e);
                //                eprintln!("{}", convert_error(&input, e));
            }

            // since this just tests that the file was parsed, there is no check whether it was
            // parsed correctly (yet)
        }
    }
}
