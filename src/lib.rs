pub mod parser_combinator {

    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while1},
        character::complete::{anychar, digit1, hex_digit1, multispace0, multispace1, one_of},
        combinator::{all_consuming, map, opt},
        error::{context, ContextError, Error, ErrorKind, ParseError, VerboseError},
        multi::{many0, many1},
        sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
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
                            dbg!(input);
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
        statements: Vec<Statement<'s>>,
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
            let (s, statements) = many0(terminated(Statement::parse_span, multispace0))(s)?;
            let (s, closed_curly_brace) = tag("}")(s)?;
            Ok((
                s,
                Self {
                    open_curly_brace,
                    statements,
                    closed_curly_brace,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum Statement<'s> {
        LetStatement(LetStatementWithSemicolon<'s>),
        IfStatement(IfStatement<'s>),
        ForLoop(ForLoop<'s>),
        CodeBlock(CodeBlock<'s>),
        Expression(ExpressionWithSemicolon<'s>),
        //InlineBssembly(InlineBssembly<'s>),
    }

    impl<'s> Statement<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(LetStatementWithSemicolon::parse_span, Self::LetStatement),
                map(IfStatement::parse_span, Self::IfStatement),
                map(ForLoop::parse_span, Self::ForLoop),
                map(CodeBlock::parse_span, Self::CodeBlock),
                map(ExpressionWithSemicolon::parse_span, Self::Expression),
                //todo map(InlineBssembly::parse_span, Self::InlineBssembly),
            ))(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct ExpressionWithSemicolon<'s> {
        expression: Option<Expression<'s>>,
        semicolon: Span<'s>,
    }

    impl<'s> ExpressionWithSemicolon<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            map(
                separated_pair(opt(Expression::parse_span), multispace0, tag(";")),
                |(expression, semicolon)| Self {
                    expression,
                    semicolon,
                },
            )(s)
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
    pub struct LetStatementWithSemicolon<'s> {
        let_statment: LetStatement<'s>,
        semicolon: Span<'s>,
    }

    impl<'s> LetStatementWithSemicolon<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            map(
                separated_pair(LetStatement::parse_span, multispace0, tag(";")),
                |(let_statment, semicolon)| Self {
                    let_statment,
                    semicolon,
                },
            )(s)
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
            let (s, condition) = Expression::parse_span(s).map_err(nom_err2failure)?;
            let (s, _) = multispace0(s)?;
            let (s, then_block) = CodeBlock::parse_span(s).map_err(nom_err2failure)?;
            let (s, _) = multispace0(s)?;
            let (s, else_token) = tag("else")(s).map_err(nom_err2failure)?;
            let (s, _) = multispace0(s)?;
            let (s, else_type) = ElseType::parse_span(s).map_err(nom_err2failure)?;
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

    #[derive(PartialEq, Eq, Debug)]
    pub struct ForLoop<'s> {
        for_token: Span<'s>,
        params: ForLoopKind<'s>,
        loop_block: CodeBlock<'s>,
    }

    impl<'s> ForLoop<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, for_token) = tag("for")(s)?;
            let (s, _) = multispace1(s)?;
            let (s, params) = ForLoopKind::parse_span(s).map_err(nom_err2failure)?;
            let (s, _) = multispace0(s)?;
            let (s, loop_block) = CodeBlock::parse_span(s).map_err(nom_err2failure)?;
            Ok((
                s,
                Self {
                    for_token,
                    params,
                    loop_block,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum ForLoopKind<'s> {
        /// for loop with `()` around the parameters
        WithBraces {
            open_brace: Span<'s>,
            params: ForLoopParams<'s>,
            closing_brace: Span<'s>,
        },
        WithoutBraces(ForLoopParams<'s>),
    }

    impl<'s> ForLoopKind<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            alt((
                map(
                    tuple((
                        terminated(tag("("), multispace0),
                        terminated(ForLoopParams::parse_span, multispace0),
                        terminated(tag(")"), multispace0),
                    )),
                    |(open_brace, params, closing_brace)| Self::WithBraces {
                        open_brace,
                        params,
                        closing_brace,
                    },
                ),
                map(
                    delimited(multispace0, ForLoopParams::parse_span, multispace0),
                    Self::WithoutBraces,
                ),
            ))(s)
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub struct ForLoopParams<'s> {
        start_assignment: Option<LetStatement<'s>>,
        semicolon1: Span<'s>,
        condition: Option<Expression<'s>>,
        semicolon2: Span<'s>,
        increment_expr: Option<Expression<'s>>,
    }

    impl<'s> ForLoopParams<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, start_assignment) = opt(LetStatement::parse_span)(s)?;
            let (s, _) = multispace0(s)?;
            let (s, semicolon1) = tag(";")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, condition) = opt(Expression::parse_span)(s)?;
            let (s, _) = multispace0(s)?;
            let (s, semicolon2) = tag(";")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, increment_expr) = opt(Expression::parse_span)(s)?;
            Ok((
                s,
                Self {
                    start_assignment,
                    semicolon1,
                    condition,
                    semicolon2,
                    increment_expr,
                },
            ))
        }
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub enum Expression<'s> {
        IntLiteral(IntLiteral<'s>),
        CharLiteral(CharLiteral<'s>),
        BinaryOperator(BinaryOperator<'s>),
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
            let (remaining, tokens) = many1(ExpressionToken::parse_span)(s)?;
            let expr = Expression::from_tokens(tokens.as_slice())?;
            Ok((remaining, expr))
        }

        fn from_tokens<
            'v,
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            values: &'v [ExpressionToken<'s>],
        ) -> Result<Self, nom::Err<E>> {
            if let [value] = values {
                match value {
                    ExpressionToken::IntLiteral(v) => Ok(Self::IntLiteral(v.clone())),
                    ExpressionToken::CharLiteral(v) => Ok(Self::CharLiteral(v.clone())),
                    ExpressionToken::Braces(_) => todo!(),
                    ExpressionToken::Operator(_) => todo!(),
                    ExpressionToken::Ident(v) => Ok(Self::Ident(v.clone())),
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

        pub fn as_ident(&self) -> Option<&Ident<'s>> {
            if let Self::Ident(v) = self {
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
        Ident(Ident<'s>),
    }

    impl<'s> ExpressionToken<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            terminated(
                alt((
                    map(IntLiteral::parse_span, Self::IntLiteral),
                    map(CharLiteral::parse_span, Self::CharLiteral),
                    map(BracesTokens::parse_span, Self::Braces),
                    map(OperatorToken::parse_span, Self::Operator),
                    map(Ident::parse_span, Self::Ident),
                )),
                multispace0,
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
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, open_brace) = tag("(")(s)?;
            let (s, _) = multispace0(s)?;
            let (s, contents) = many0(ExpressionToken::parse_span)(s)?;
            let (s, _) = multispace0(s)?;
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
            const fn operator_to_bind_strength(o: &OperatorToken<'_>) -> u8 {
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
                    OperatorToken::Comma(_) => 0,
                }
            }

            operator_to_bind_strength(self).partial_cmp(&operator_to_bind_strength(other))
        }
    }

    impl<'s> OperatorToken<'s> {
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
        fn from_tokens<'v, E>(
            tokens: &'v [ExpressionToken<'s>],
        ) -> Result<Option<Self>, nom::Err<E>>
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
        fn from_tokens<
            'v,
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
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
        fn from_tokens<
            'v,
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
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

    #[derive(PartialEq, Eq, Debug, Clone)]
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

    #[derive(PartialEq, Eq, Debug, Clone)]
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

    #[derive(PartialEq, Eq, Debug, Clone)]
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

    #[derive(PartialEq, Eq, Debug, Clone)]
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

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct Ident<'s>(Span<'s>);
    impl<'s> Ident<'s> {
        pub fn parse_span<
            E: ParseError<LocatedSpan<&'s str>> + ContextError<LocatedSpan<&'s str>>,
        >(
            s: Span<'s>,
        ) -> IResult<Span, Self, E> {
            let (s, ident) = take_while1(AsChar::is_alphanum)(s)?;
            //todo: check that ident does not start with a number
            //
            //check that ident is not a reserved word
            assert!("if" != *ident);
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

    fn nom_err2failure<E>(err: nom::Err<E>) -> nom::Err<E> {
        match err {
            nom::Err::Error(e) => nom::Err::Failure(e),
            _ => err,
        }
    }

    #[cfg(test)]
    mod tests {
        use std::fs::read_to_string;

        use super::*;
        use nom::{combinator::eof, error::VerboseError};
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

        #[test]
        fn test_operators() {
            let input = "b == c";
            let span = LocatedSpan::new(input);
            let (remaining, tokens) =
                terminated(many1(ExpressionToken::parse_span::<VerboseError<_>>), eof)(span)
                    .unwrap();
            dbg!(&tokens);
            let parsed = BinaryOperator::from_tokens::<VerboseError<_>>(tokens.as_slice())
                .unwrap()
                .unwrap();
            let operator_token = parsed.operator;
            assert!(matches!(operator_token, OperatorToken::Eq(_)));
            let lhs = parsed.lhs.as_ident().unwrap();
            assert_eq!("b", <&str>::from(lhs));
            let rhs = parsed.rhs.as_ident().unwrap();
            assert_eq!("c", <&str>::from(rhs));
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
