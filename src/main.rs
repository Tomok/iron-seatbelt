mod parser_combinator {

    use nom::{
        bytes::complete::{tag, take_till, take_until, take_while, take_while1},
        character::{
            complete::{multispace0, multispace1},
            is_alphanumeric, is_newline, is_space,
        },
        multi::many0,
        AsChar, IResult,
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
            dbg!(&function);
            let (s, _) = multispace1(s)?;
            let (s, name) = Ident::parse_span(s)?;
            dbg!(&name);
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
        //todo
        closing_brace: Span<'s>,
    }

    impl<'s> Parameters<'s> {
        pub fn parse_span(s: Span<'s>) -> IResult<Span, Self> {
            let (s, open_brace) = tag("(")(s)?;
            let (s, _) = multispace0(s)?;
            //todo
            let (s, _) = multispace0(s)?;
            let (s, closing_brace) = tag(")")(s)?;
            Ok((
                s,
                Self {
                    open_brace,
                    closing_brace,
                },
            ))
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
            let (s, ident) = take_while1(AsChar::is_alpha)(s)?; //todo: allow for numbers in idents
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
        use super::*;
        use textwrap::dedent;
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
    }
}

fn main() {
    println!("Hello, world!");
}
