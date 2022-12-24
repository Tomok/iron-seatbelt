mod parser_combinator {

    use nom::{
        bytes::complete::{tag, take_till, take_until, take_while, take_while1},
        character::{
            complete::{multispace0, multispace1},
            is_alphanumeric, is_newline, is_space,
        },
        error::{Error, ErrorKind},
        multi::many0,
        AsChar, IResult, InputTakeAtPosition,
    };
    use nom_locate::{position, LocatedSpan};

    type Span<'a> = LocatedSpan<&'a str>;

    #[derive(PartialEq, Eq, Debug)]
    pub struct BackSeatFile<'s> {
        entries: Vec<FileEntry<'s>>,
    }

    impl<'s> BackSeatFile<'s> {
        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
            let (s, entries) = many0(FileEntry::parse_span)(s)?;
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
        Import, //todo
        Function(Function<'s>),
    }

    impl<'s> FileEntry<'s> {
        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
            //todo Import
            let (s, f) = Function::parse_span(s)?;
            Ok((s, Self::Function(f)))
        }
    }

    impl<'s> FileEntry<'s> {
        /// Returns `true` if the file entry is [`Function`].
        ///
        /// [`Function`]: FileEntry::Function
        #[must_use]
        pub fn is_function(&self) -> bool {
            matches!(self, Self::Function(..))
        }

        /// Returns `true` if the file entry is [`Import`].
        ///
        /// [`Import`]: FileEntry::Import
        #[must_use]
        pub fn is_import(&self) -> bool {
            matches!(self, Self::Import)
        }

        pub fn as_function(&self) -> Option<&Function<'s>> {
            if let Self::Function(v) = self {
                Some(v)
            } else {
                None
            }
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

        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
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
        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
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
                            return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)));
                        }
                        let (s, param) = Parameter::parse_span(input)?;
                        next_param_allowed = param.comma.is_some();
                        params.push(param);
                        let (s, _) = multispace0(s)?;
                        read_pos = s;
                    }
                    Err(e) => {
                        return Err(e); //raise every other error
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
        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
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

        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
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
    pub struct Ident<'s>(Span<'s>);
    impl<'s> Ident<'s> {
        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
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
        let input = LocatedSpan::new(input);
        match BackSeatFile::parse_span(input) {
            Ok((_, bs)) => Ok(bs),
            Err(e) => todo!(),
        }
    }

    #[cfg(test)]
    mod tests {
        use std::fs::read_to_string;

        use super::*;
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
            let (_, bs_file) = BackSeatFile::parse_span(LocatedSpan::new(&input)).unwrap(); //parse(&input).unwrap();
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
            let _bs_file = parse(&script).unwrap();
            // since this just tests that the file was parsed, there is no check whether it was
            // parsed correctly (yet)
        }
    }
}

fn main() {
    println!("Hello, world!");
}
