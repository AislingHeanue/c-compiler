use std::{
    collections::{HashMap, HashSet, VecDeque},
    error::Error,
    fs,
    path::PathBuf,
};

use chrono::Local;
use conditional::{prepare_tokens_for_if, resolve_number};
use itertools::Itertools;
use lazy_static::lazy_static;
use macros::resolve_identifier;

use super::{
    internal_preprocess,
    lexer::{PreprocessorToken, Tokens},
};

mod conditional;
mod macros;

lazy_static! {
    static ref PREDEFINED_MACROS: HashMap<String,Macro> = HashMap::from([
        ("__DATE__".to_string(),                    Macro::string_constant_from(format!(r#""{}""#, Local::now().format("%b %d %y")).as_str())),
        ("__TIME__".to_string(),                    Macro::string_constant_from(format!(r#""{}""#, Local::now().format("%X")).as_str())),
        ("__STDC__".to_string(),                    Macro::from_number(1)),
        ("__STDC_VERSION__".to_string(),            Macro::Plain(vec![PreprocessorToken::Number("201710L".to_string())])),
        // signifies that the target should have access to the C standard library
        // while running this file. eg. this might not be true when targeting embedded
        // devices.
        ("__STDC_HOSTED__".to_string(),             Macro::from_number(1)),
        // optional extensions
        ("__REGISTER_PREFIX__".to_string(),         Macro::from("%")),
        ("__SIZE_TYPE__".to_string(),               Macro::from("unsigned long")),
        ("__PTRDIFF_TYPE__".to_string(),            Macro::from("long")),
        ("__WCHAR_TYPE__".to_string(),              Macro::from("char")),
        ("__WINT_TYPE__".to_string(),               Macro::from("unsigned int")),
        ("__INTMAX_TYPE__".to_string(),             Macro::from("long long")),
        ("__UINTMAX_TYPE__".to_string(),            Macro::from("unsigned long long")),
        // https://stackoverflow.com/questions/24931456/how-does-sig-atomic-t-actually-work
        ("__SIG_ATOMIC_TYPE__".to_string(),         Macro::from("int")),
        ("__INT8_TYPE__".to_string(),               Macro::from("char")),
        ("__INT16_TYPE__".to_string(),              Macro::from("short")),
        ("__INT32_TYPE__".to_string(),              Macro::from("int")),
        ("__INT64_TYPE__".to_string(),              Macro::from("long")),
        ("__UINT8_TYPE__".to_string(),              Macro::from("unsigned char")),
        ("__UINT16_TYPE__".to_string(),             Macro::from("char")),
        ("__UINT32_TYPE__".to_string(),             Macro::from("unsigned int")),
        ("__UINT64_TYPE__".to_string(),             Macro::from("unsigned long")),
        ("__INT_LEAST8_TYPE__".to_string(),         Macro::from("char")),
        ("__INT_LEAST16_TYPE__".to_string(),        Macro::from("short")),
        ("__INT_LEAST32_TYPE__".to_string(),        Macro::from("int")),
        ("__INT_LEAST64_TYPE__".to_string(),        Macro::from("long")),
        ("__UINT_LEAST8_TYPE__".to_string(),        Macro::from("unsigned char")),
        ("__UINT_LEAST16_TYPE__".to_string(),       Macro::from("unsigned short")),
        ("__UINT_LEAST32_TYPE__".to_string(),       Macro::from("unsigned int")),
        ("__UINT_LEAST64_TYPE__".to_string(),       Macro::from("unsigned long")),
        ("__INT_FAST8_TYPE__".to_string(),          Macro::from("char")),
        ("__INT_FAST16_TYPE__".to_string(),         Macro::from("long")),
        ("__INT_FAST32_TYPE__".to_string(),         Macro::from("long")),
        ("__INT_FAST64_TYPE__".to_string(),         Macro::from("long")),
        ("__UINT_FAST8_TYPE__".to_string(),         Macro::from("unsigned char")),
        ("__UINT_FAST16_TYPE__".to_string(),        Macro::from("unsigned long")),
        ("__UINT_FAST32_TYPE__".to_string(),        Macro::from("unsigned long")),
        ("__UINT_FAST64_TYPE__".to_string(),        Macro::from("unsigned long")),
        ("__INTPTR_TYPE__".to_string(),             Macro::from("long")),
        ("__UNINTPTR_TYPE__".to_string(),           Macro::from("unsigned long")),
        ("__CHAR_BIT__".to_string(),                Macro::from_number(8)),
        ("__SCHAR_MAX__".to_string(),               Macro::from_number(i8::MAX.into())),
        ("__WCHAR_MAX__".to_string(),               Macro::from_number(i8::MAX.into())),
        ("__SHRT_MAX__".to_string(),                Macro::from_number(i16::MAX.into())),
        ("__INT_MAX__".to_string(),                 Macro::from_number(i32::MAX.into())),
        ("__LONG_MAX__".to_string(),                Macro::from_number(i64::MAX)),
        ("__LONG_MAX__".to_string(),                Macro::from_number(i64::MAX)),
        ("__LONG_LONG_MAX__".to_string(),           Macro::from_u64_number(u64::MAX)),
        ("__WINT_MAX__".to_string(),                Macro::from_number(u32::MAX.into())),
        ("__SIZE_MAX__".to_string(),                Macro::from_u64_number(u64::MAX)),
        ("__PTRDIFF_MAX__".to_string(),             Macro::from_number(i64::MAX)),
        ("__INTMAX_MAX__".to_string(),              Macro::from_number(i64::MAX)),
        ("__UINTMAX_MAX__".to_string(),             Macro::from_u64_number(u64::MAX)),
        ("__INT8_MAX__".to_string(),                Macro::from_number(i8::MAX.into())),
        ("__INT16_MAX__".to_string(),               Macro::from_number(i16::MAX.into())),
        ("__INT32_MAX__".to_string(),               Macro::from_number(i32::MAX.into())),
        ("__INT64_MAX__".to_string(),               Macro::from_number(i64::MAX)),
        ("__UINT8_MAX__".to_string(),               Macro::from_number(u8::MAX.into())),
        ("__UINT16_MAX__".to_string(),              Macro::from_number(u16::MAX.into())),
        ("__UINT32_MAX__".to_string(),              Macro::from_number(u32::MAX.into())),
        ("__UINT64_MAX__".to_string(),              Macro::from_u64_number(u64::MAX)),
        ("__INT_LEAST8_MAX__".to_string(),          Macro::from_number(i8::MAX.into())),
        ("__INT_LEAST16_MAX__".to_string(),         Macro::from_number(i16::MAX.into())),
        ("__INT_LEAST32_MAX__".to_string(),         Macro::from_number(i32::MAX.into())),
        ("__INT_LEAST64_MAX__".to_string(),         Macro::from_number(i64::MAX)),
        ("__UINT_LEAST8_MAX__".to_string(),         Macro::from_number(u8::MAX.into())),
        ("__UINT_LEAST16_MAX__".to_string(),        Macro::from_number(u16::MAX.into())),
        ("__UINT_LEAST32_MAX__".to_string(),        Macro::from_number(u32::MAX.into())),
        ("__UINT_LEAST64_MAX__".to_string(),        Macro::from_u64_number(u64::MAX)),
        ("__INT_FAST8_MAX__".to_string(),           Macro::from_number(i8::MAX.into())),
        ("__INT_FAST16_MAX__".to_string(),          Macro::from_number(i64::MAX)),
        ("__INT_FAST32_MAX__".to_string(),          Macro::from_number(i64::MAX)),
        ("__INT_FAST64_MAX__".to_string(),          Macro::from_number(i64::MAX)),
        ("__UINT_FAST8_MAX__".to_string(),          Macro::from_number(u8::MAX.into())),
        ("__UINT_FAST16_MAX__".to_string(),         Macro::from_u64_number(u64::MAX)),
        ("__UINT_FAST32_MAX__".to_string(),         Macro::from_u64_number(u64::MAX)),
        ("__UINT_FAST64_MAX__".to_string(),         Macro::from_u64_number(u64::MAX)),
        ("__INTPTR_MAX__".to_string(),              Macro::from_number(i64::MAX)),
        ("__UNINTPTR_MAX__".to_string(),            Macro::from_u64_number(u64::MAX)),
        ("__WCHAR_MIN__".to_string(),               Macro::from_number(i8::MIN.into())),
        ("__WINT_MIN__".to_string(),                Macro::from_number(i32::MIN.into())),
        ("__SIG_ATOMIC_MIN__".to_string(),          Macro::from_number(i32::MIN.into())),
        ("__INT8_C".to_string(),                    Macro::Function(vec![("c".to_string(),true)], vec![PreprocessorToken::Identifier("c".to_string())])),
        ("__INT16_C".to_string(),                   Macro::Function(vec![("c".to_string(), true)], vec![PreprocessorToken::Identifier("c".to_string())])),
        ("__INT32_C".to_string(),                   Macro::Function(vec![("c".to_string(), true)], vec![PreprocessorToken::Identifier("c".to_string())])),
        ("__INT64_C".to_string(),                   Macro::Function(vec![("c".to_string(), false)], vec![
            PreprocessorToken::Identifier("c".to_string()),
            PreprocessorToken::WidePunctuator("##".to_string()),
            PreprocessorToken::Identifier("L".to_string()),
        ])),
        ("__UINT8_C".to_string(),                   Macro::Function(vec![("c".to_string(), true)], vec![PreprocessorToken::Identifier("c".to_string())])),
        ("__UINT16_C".to_string(),                  Macro::Function(vec![("c".to_string(), true)], vec![PreprocessorToken::Identifier("c".to_string())])),
        ("__UINT32_C".to_string(),                  Macro::Function(vec![("c".to_string(), false)], vec![
            PreprocessorToken::Identifier("c".to_string()),
            PreprocessorToken::WidePunctuator("##".to_string()),
            PreprocessorToken::Identifier("U".to_string()),
        ])),
        ("__UINT64_C".to_string(),                  Macro::Function(vec![("c".to_string(), false)], vec![
            PreprocessorToken::Identifier("c".to_string()),
            PreprocessorToken::WidePunctuator("##".to_string()),
            PreprocessorToken::Identifier("UL".to_string()),
        ])),
        ("__INTMAX_C".to_string(),                  Macro::Function(vec![("c".to_string(), false)], vec![
            PreprocessorToken::Identifier("c".to_string()),
            PreprocessorToken::WidePunctuator("##".to_string()),
            PreprocessorToken::Identifier("L".to_string()),
        ])),
        ("__UINTMAX_C".to_string(),                 Macro::Function(vec![("c".to_string(), false)], vec![
            PreprocessorToken::Identifier("c".to_string()),
            PreprocessorToken::WidePunctuator("##".to_string()),
            PreprocessorToken::Identifier("UL".to_string()),
        ])),
        ("__SCHAR_WIDTH__".to_string(),             Macro::from_number(1)),
        ("__SHRT_WIDTH__".to_string(),              Macro::from_number(2)),
        ("__INT_WIDTH__".to_string(),               Macro::from_number(4)),
        ("__LONG_WIDTH__".to_string(),              Macro::from_number(8)),
        ("__LONG_LONG_WIDTH__".to_string(),         Macro::from_number(8)),
        ("__PTRDIFF_WIDTH__".to_string(),           Macro::from_number(8)),
        ("__SIG_ATOMIC_WIDTH__".to_string(),        Macro::from_number(4)),
        ("__SIZE_WIDTH__".to_string(),              Macro::from_number(8)),
        ("__WCHAR_WIDTH__".to_string(),             Macro::from_number(1)),
        ("__WINT_WIDTH__".to_string(),              Macro::from_number(4)),
        ("__INT_LEAST8_WIDTH__".to_string(),        Macro::from_number(1)),
        ("__INT_LEAST16_WIDTH__".to_string(),       Macro::from_number(2)),
        ("__INT_LEAST32_WIDTH__".to_string(),       Macro::from_number(4)),
        ("__INT_LEAST64_WIDTH__".to_string(),       Macro::from_number(8)),
        ("__INT_FAST8_WIDTH__".to_string(),         Macro::from_number(1)),
        ("__INT_FAST16_WIDTH__".to_string(),        Macro::from_number(2)),
        ("__INT_FAST32_WIDTH__".to_string(),        Macro::from_number(4)),
        ("__INT_FAST64_WIDTH__".to_string(),        Macro::from_number(8)),
        ("__INTPTR_WIDTH__".to_string(),            Macro::from_number(8)),
        ("__INTMAX_WIDTH__".to_string(),            Macro::from_number(8)),
        ("__SIZEOF_INT__".to_string(),              Macro::from_number(4)),
        ("__SIZEOF_LONG__".to_string(),             Macro::from_number(8)),
        ("__SIZEOF_LONG_LONG__".to_string(),        Macro::from_number(8)),
        ("__SIZEOF_SHORT__".to_string(),            Macro::from_number(2)),
        ("__SIZEOF_POINTER__".to_string(),          Macro::from_number(8)),
        ("__SIZEOF_FLOAT__".to_string(),            Macro::from_number(4)),
        ("__SIZEOF_DOUBLE__".to_string(),           Macro::from_number(8)),
        ("__SIZEOF_LONG_DOUBLE__".to_string(),      Macro::from_number(8)),
        ("__SIZEOF_SIZE_T__".to_string(),           Macro::from_number(8)),
        ("__SIZEOF_WCHAR_T__".to_string(),          Macro::from_number(1)),
        ("__SIZEOF_WINT_T__".to_string(),           Macro::from_number(4)),
        ("__SIZEOF_PTRDIFF_T__".to_string(),        Macro::from_number(8)),
        ("__BYTE_ORDER__".to_string(),              Macro::from("__ORDER_LITTLE_ENDIAN__")),
        ("__FLOAT_WORD_ORDER__".to_string(),        Macro::from("__ORDER_LITTLE_ENDIAN__")),
        // set to 1 on all but very VERY old C versions (obsolete)
        ("__GXX_EXPERIMENTAL_CXX0X__".to_string(),  Macro::from_number(1)),
        // signifies that ints are 32 bits and long and long long are 64 bits
        ("__LP64__".to_string(),                    Macro::from_number(1)),
        ("__LP".to_string(),                        Macro::from_number(1)),
        // signifies this is compiling for unix
        ("__unix__".to_string(),                    Macro::from_number(1)),
        ("__x86_64__".to_string(),                  Macro::from_number(1)),
        // _ on mac
        ("__USER_LABEL_PREFIX__".to_string(),       Macro::Plain(vec![])),
    ]);
}

struct InterpreterContext {
    // identifier_locs: HashMap<String, HashSet<(usize, usize)>>,
    macros: HashMap<String, Macro>,
    default_include_paths: [PathBuf; 3],
    current_dir: PathBuf,
    line_num: usize,
    filename: String,
    include_nesting_count: usize,
}

#[derive(Clone, Debug)]
pub enum Macro {
    Plain(Vec<PreprocessorToken>),
    // function name, param names, tokens in function body (maybe could be string?)
    Function(Vec<(String, bool)>, Vec<PreprocessorToken>),
    Undef,
}

impl Macro {
    // only returns plain macros, helper function for the above map
    fn from(s: &str) -> Macro {
        let mut out_tokens = Vec::new();
        // banish all whitespace while we're here
        for word in s.split_whitespace() {
            out_tokens.push(PreprocessorToken::Identifier(word.to_string()));
            out_tokens.push(PreprocessorToken::WhiteSpace(" ".to_string()));
        }

        out_tokens.pop();

        Macro::Plain(out_tokens)
    }

    fn from_number(s: i64) -> Macro {
        Macro::Plain(vec![PreprocessorToken::Number(s.to_string())])
    }

    fn from_u64_number(s: u64) -> Macro {
        Macro::Plain(vec![PreprocessorToken::Number(s.to_string())])
    }

    fn string_constant_from(s: &str) -> Macro {
        Macro::Plain(vec![PreprocessorToken::StringLiteral(
            PreprocessorToken::parse_string(s.to_string()),
        )])
    }
}

impl InterpreterContext {
    fn new(
        filename: &str,
        nesting_count: usize,
        macros: HashMap<String, Macro>,
    ) -> Result<InterpreterContext, Box<dyn Error>> {
        let mut current_path = PathBuf::from(filename);
        current_path = fs::canonicalize(current_path)?;
        // get the directory containing filename, not filename itself
        current_path.pop();

        Ok(InterpreterContext {
            // take macros from the parent context if we're in an include directive, else use
            // default
            macros: if nesting_count == 0 {
                PREDEFINED_MACROS.clone()
            } else {
                macros
            },
            // required by C spec.
            line_num: 0,
            // TODO: -I flag to prepend system file paths
            default_include_paths: [
                // NOTE: for the moment, this means the that compiler MUST be run from the root of
                // this repository, because otherwise the path for include will be missing if you
                // try to import stddef.h or stdarg.h
                PathBuf::from("include"),
                PathBuf::from("/usr/include"),
                PathBuf::from("/usr/local/include"),
            ],
            current_dir: current_path,
            filename: filename.to_string(),
            include_nesting_count: nesting_count,
        })
    }
}

pub fn interpret(
    filename: String,
    tokens: Tokens,
    nesting_count: usize,
    macros: HashMap<String, Macro>,
) -> Result<(String, HashMap<String, Macro>), Box<dyn Error>> {
    // println!("INTERPRETING {}", filename);
    let mut context = InterpreterContext::new(&filename, nesting_count, macros)?;

    // collect_identifiers(&tokens, &mut context);

    let mut jump_to_else = false;
    let mut jump_to_end = false;
    let mut current_if_nesting = 0;
    let mut out = "".to_string();
    let mut line_iter = tokens.0.iter().peekable();
    let mut current_running_blank_lines = 1;
    while let Some(line) = line_iter.next() {
        // lines are 1-indexed
        context.line_num += 1;

        // FLOW CONTROL
        if jump_to_else || jump_to_end {
            if matches!(
                line.first(),
                Some(
                    PreprocessorToken::DirectiveIfdef
                        | PreprocessorToken::DirectiveIfndef
                        | PreprocessorToken::DirectiveIf
                )
            ) {
                current_if_nesting += 1;
            }
            if matches!(line.first(), Some(PreprocessorToken::DirectiveEndif)) {
                current_if_nesting -= 1;
            }
        }

        // if encountering a wild else while not jumping elsewhere, skip it and jump to the next
        // end
        if !jump_to_else
            && !jump_to_end
            && matches!(
                line.first(),
                Some(PreprocessorToken::DirectiveElse | PreprocessorToken::DirectiveElif)
            )
        {
            current_if_nesting = 1;
            jump_to_end = true;
            continue;
        }

        // jump to the next (not nested) endif or else
        if jump_to_else {
            if current_if_nesting == 0
                && matches!(line.first(), Some(PreprocessorToken::DirectiveEndif))
            {
                jump_to_else = false;
                continue;
            } else if current_if_nesting == 1
                && matches!(
                    line.first(),
                    Some(PreprocessorToken::DirectiveElse | PreprocessorToken::DirectiveElif)
                )
            {
                jump_to_else = false;
            } else {
                continue;
            }
        }

        // jump to the next (not nested) endif
        if jump_to_end {
            if current_if_nesting == 0
                && matches!(line.first(), Some(PreprocessorToken::DirectiveEndif))
            {
                jump_to_end = false;
            }
            continue;
        }

        jump_to_else = false;
        jump_to_end = false;

        let mut expect_end_of_line = false;
        let mut last_directive_was_include = false;
        let mut token_iter = line.iter().peekable();
        let mut line_text = "".to_string();
        while let Some(token) = token_iter.next() {
            if !matches!(token, PreprocessorToken::WhiteSpace(_)) && expect_end_of_line {
                return Err(format!("Extra token found after directive: {}", token).into());
            }
            match token {
                // these aren't directives, ignore them
                // DirectiveIncludeWithMacro followed by StringLiteral is not possible since that
                // would result in an Include(String) instead
                PreprocessorToken::StringLiteral(_)
                | PreprocessorToken::Number(_)
                | PreprocessorToken::CharacterConstant(_)
                | PreprocessorToken::Punctuator(_)
                | PreprocessorToken::WidePunctuator(_)
                | PreprocessorToken::WhiteSpace(_)
                | PreprocessorToken::KeywordDefined => line_text += token.to_string().as_str(),

                PreprocessorToken::Identifier(s) => {
                    //TODO: add __func__ to the main compiler
                    if s == "__LINE__" {
                        line_text += &context.line_num.to_string();
                    } else if s == "__FILE__" {
                        line_text += &context.filename.to_string();
                    } else {
                        let new_tokens;
                        // we need to pass the ENTIRE iterators in here because this function
                        // may need to pop further tokens off the iterator to resolve function
                        // calls, which for SOME GODFORSAKEN REASON can span multiple lines despite
                        // almost nothing else allowing it.
                        (new_tokens, token_iter, line_iter) = resolve_identifier(
                            vec![token.clone()].into(),
                            token_iter,
                            line_iter,
                            HashSet::new(),
                            &mut context,
                        )?;

                        // let new_tokens = resolve_string(s, &mut context)?;
                        for token in new_tokens {
                            line_text += &token.to_string();
                        }
                    }
                }

                PreprocessorToken::DirectiveDefine => {
                    let k = if let Some(PreprocessorToken::Identifier(s)) = token_iter.next() {
                        s
                    } else {
                        return Err("#define must be followed by an identifier".into());
                    };
                    match token_iter.peek() {
                        Some(PreprocessorToken::Punctuator(s)) if s == "(" => {
                            // TODO: function-like macro definitions and evaluation (several
                            // non-intuitive steps involved)

                            // consume the ( token
                            token_iter.next();
                            let mut params = Vec::new();
                            let mut body_tokens = Vec::new();

                            let mut parsing_params = true;
                            let mut expecting_identifier = true;
                            let mut trailing_comma = false;
                            for t in token_iter.by_ref() {
                                if parsing_params {
                                    match t {
                                        PreprocessorToken::Punctuator(s)
                                            if *s == ")" && !trailing_comma =>
                                        {
                                            parsing_params = false;
                                        }
                                        PreprocessorToken::Identifier(s)
                                            if expecting_identifier =>
                                        {
                                            expecting_identifier = false;
                                            trailing_comma = false;
                                            let new_param = (s.clone(), true);
                                            if params.contains(&new_param) {
                                                return Err(format!("Duplicate parameter name in macro definition: {}", new_param.0).into());
                                            }
                                            params.push(new_param);
                                        }
                                        PreprocessorToken::Punctuator(s)
                                            if *s == "," && !expecting_identifier =>
                                        {
                                            expecting_identifier = true;
                                            trailing_comma = true;
                                        }
                                        PreprocessorToken::WhiteSpace(_) => {}
                                        _ => {
                                            // println!("{:?} {:?} {:?}", token, k, t);
                                            return Err(
                                                "Invalid params for functional macro".into()
                                            );
                                        }
                                    }
                                } else {
                                    // trim whitespace from start of body
                                    if body_tokens.is_empty()
                                        && matches!(t, PreprocessorToken::WhiteSpace(_))
                                    {
                                        continue;
                                    }
                                    body_tokens.push(t.clone());
                                }
                            }
                            // if the body contains any #params or _ ## _, mark should_prescan for
                            // those params to be false
                            //
                            // kill any whitespace before or after ##
                            let mut indexes_to_remove = Vec::new();
                            for i in 0..body_tokens.len() {
                                match body_tokens.get(i).unwrap() {
                                    PreprocessorToken::Punctuator(s) if s == "#" => {
                                        if let Some(PreprocessorToken::Identifier(s)) =
                                            body_tokens.get(i + 1)
                                        {
                                            if !params.iter().any(|(p, _)| p == s) {
                                                return Err("# in function-like macro body should be a parameter name".into());
                                            }
                                            do_not_prescan(&mut params, s);
                                        } else {
                                            return Err("# in function-like macro body should be followed by an identifier".into());
                                        }
                                    }
                                    PreprocessorToken::WidePunctuator(s) if s == "##" => {
                                        let mut previous_index = i - 1;
                                        while let Some(PreprocessorToken::WhiteSpace(_)) =
                                            body_tokens.get(previous_index)
                                        {
                                            indexes_to_remove.push(previous_index);
                                            previous_index -= 1;
                                        }
                                        if body_tokens.get(previous_index).is_none() {
                                            return Err("## cannot appear at the start of a function-like macro body".into());
                                        }
                                        if let Some(PreprocessorToken::Identifier(s)) =
                                            body_tokens.get(previous_index)
                                        {
                                            do_not_prescan(&mut params, s);
                                        }

                                        let mut next_index = i + 1;
                                        while let Some(PreprocessorToken::WhiteSpace(_)) =
                                            body_tokens.get(next_index)
                                        {
                                            indexes_to_remove.push(next_index);
                                            next_index += 1;
                                        }
                                        if body_tokens.get(next_index).is_none() {
                                            return Err("## cannot appear at the end of a function-like macro body".into());
                                        }
                                        if let Some(PreprocessorToken::Identifier(s)) =
                                            body_tokens.get(next_index)
                                        {
                                            do_not_prescan(&mut params, s);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            for index in indexes_to_remove.iter().unique().sorted().rev() {
                                body_tokens.remove(*index);
                            }

                            // TODO: this warning should only occur if the replacement
                            // tokens/params differ, not counting variable whitespace sizes
                            // see section 3.8 of https://gcc.gnu.org/onlinedocs/cpp.pdf
                            if context.macros.contains_key(k) {
                                // println!("WARNING: Macro '{}' was re-defined", k);
                            }
                            // println!("DEFINING {} {:?} -> {:?}", k, params, body_tokens);
                            context
                                .macros
                                .insert(k.clone(), Macro::Function(params, body_tokens));
                        }
                        _ => {
                            let mut v = Vec::new();
                            for t in token_iter.by_ref() {
                                if v.is_empty() && matches!(t, PreprocessorToken::WhiteSpace(_)) {
                                    continue;
                                }
                                v.push(t.clone())
                            }
                            if context.macros.contains_key(k) {
                                // println!("WARNING: Macro '{}' was re-defined", k);
                            }
                            context.macros.insert(k.clone(), Macro::Plain(v));
                        }
                    }
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveUndef => {
                    let mut next_token = token_iter.next();
                    while let Some(PreprocessorToken::WhiteSpace(_)) = next_token {
                        next_token = token_iter.next();
                    }
                    if let Some(PreprocessorToken::Identifier(s)) = next_token {
                        if let Some(m) = context.macros.get_mut(s) {
                            *m = Macro::Undef;
                        }
                    } else {
                        return Err(format!(
                            "Invalid token found after undef directive, expecting identifier but got {:?}",token)
                                .into(),
                        );
                    }
                    expect_end_of_line = true;
                }

                PreprocessorToken::DirectiveInclude(inner_text, has_angle_brackets) => {
                    // resolve_include
                    line_text +=
                        resolve_include(inner_text, *has_angle_brackets, &mut context)?.as_str();
                    expect_end_of_line = true;
                    last_directive_was_include = true;
                }
                PreprocessorToken::DirectiveIncludeWithMacro => {
                    if let Some(PreprocessorToken::Identifier(_)) = token_iter.next() {
                        // get the string representation of the tokens here because we can't treat
                        // "" as a plain string literal any more
                        let new_tokens;
                        (new_tokens, token_iter, line_iter) = resolve_identifier(
                            vec![token.clone()].into(),
                            token_iter,
                            line_iter,
                            HashSet::new(),
                            &mut context,
                        )?;

                        let new_s: String = new_tokens.into_iter().map(|t| t.to_string()).collect();
                        let (has_angle_brackets, inner_text) = if new_s.starts_with("<")
                            && new_s.ends_with(">")
                        {
                            (true, new_s[1..new_s.len()].trim_end())
                        } else if new_s.starts_with("\"") && new_s.ends_with("\"") {
                            (true, &new_s[1..new_s.len()])
                        } else {
                            return Err(
                                format!(
                                    "Value after #include did not resolve to a valid expression in the form \"something\" or <something>, got: {}",
                                    new_s
                                ).into()
                            );
                        };
                        line_text +=
                            resolve_include(inner_text, has_angle_brackets, &mut context)?.as_str();
                        expect_end_of_line = true;
                        last_directive_was_include = true;
                    } else {
                        return Err("Invalid token found after include directive, expecting identifier or string literal".into());
                    }
                }

                PreprocessorToken::DirectiveIfdef => {
                    current_if_nesting = 1;
                    if let Some(PreprocessorToken::Identifier(s)) = token_iter.next() {
                        if let Some(Macro::Undef) | None = context.macros.get(s) {
                            jump_to_else = true;
                        }
                    } else {
                        return Err(
                            "Invalid token found after ifdef directive, expecting identifier"
                                .into(),
                        );
                    }
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveIfndef => {
                    current_if_nesting = 1;
                    if let Some(PreprocessorToken::Identifier(s)) = token_iter.next() {
                        if let Some(Macro::Function(_, _) | Macro::Plain(_)) = context.macros.get(s)
                        {
                            jump_to_else = true;
                        }
                    } else {
                        return Err(
                            "Invalid token found after ifdef directive, expecting identifier"
                                .into(),
                        );
                    }
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveIf => {
                    current_if_nesting = 1;
                    let if_tokens;
                    (if_tokens, token_iter, line_iter) =
                        prepare_tokens_for_if(token_iter, line_iter, &mut context)?;
                    let number = resolve_number(if_tokens, &mut context)?;
                    if number == 0 {
                        jump_to_else = true;
                    }
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveElif => {
                    // current_if_nesting = 1; THIS IS NOT TRUE
                    let if_tokens;
                    (if_tokens, token_iter, line_iter) =
                        prepare_tokens_for_if(token_iter, line_iter, &mut context)?;
                    let number = resolve_number(if_tokens, &mut context)?;
                    if number == 0 {
                        jump_to_else = true;
                    }
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveElse => {
                    expect_end_of_line = true;
                } // no-op, just a target for flow-control
                PreprocessorToken::DirectiveEndif => {
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveLine => {
                    let mut tokens = Vec::new();
                    for t in token_iter.by_ref() {
                        tokens.push(t.clone());
                    }
                    let mut number = None;
                    let mut filename = None;
                    let mut invalid_token_found = false;
                    for t in tokens.iter() {
                        match t {
                            PreprocessorToken::Number(s) if number.is_none() => {
                                number = Some(s.parse::<usize>()?)
                            }
                            PreprocessorToken::StringLiteral(s)
                                if number.is_some() && filename.is_none() =>
                            {
                                filename = Some(s.clone())
                            }
                            _ => {
                                invalid_token_found = true;
                                break;
                            }
                        }
                    }
                    if invalid_token_found || number.is_none() {
                        //= resolve_identifier(tokens.clone(), HashSet::new(), &mut context)?;

                        let new_tokens;
                        (new_tokens, token_iter, line_iter) = resolve_identifier(
                            vec![token.clone()].into(),
                            token_iter,
                            line_iter,
                            HashSet::new(),
                            &mut context,
                        )?;

                        number = None;
                        filename = None;
                        for t in new_tokens.iter() {
                            match t {
                                PreprocessorToken::Number(s) if number.is_none() => {
                                    number = Some(s.parse::<usize>()?)
                                }
                                PreprocessorToken::StringLiteral(s)
                                    if number.is_some() && filename.is_none() =>
                                {
                                    filename = Some(s.clone())
                                }
                                _ => {
                                    invalid_token_found = true;
                                    break;
                                }
                            }
                        }
                    }
                    if invalid_token_found || number.is_none() {
                        return Err(
                            format!(
                                "Could not interpret #line directive in the form 'linenum [filename]', got: {}",
                                Tokens(vec![tokens])
                            ).into()
                        );
                    };
                    // this line number is meant to apply to the NEXT line, so subtract 1
                    context.line_num = number.unwrap();
                    if let Some(f) = filename {
                        context.filename = PreprocessorToken::StringLiteral(f).to_string();
                    }
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectiveError => {
                    let mut out_string = "".to_string();
                    for t in token_iter.by_ref() {
                        out_string += format!("{}", t).as_str();
                    }

                    return Err(out_string.into());
                }
                PreprocessorToken::DirectiveWarning => {
                    let mut out_string = "".to_string();
                    for t in token_iter.by_ref() {
                        out_string += format!("{}", t).as_str();
                    }
                    println!("WARNING: {}", out_string);
                    expect_end_of_line = true;
                }
                PreprocessorToken::DirectivePragma => {
                    let mut out_string = "".to_string();
                    for t in token_iter.by_ref() {
                        out_string += format!("{}", t).as_str();
                    }
                    println!(
                        "WARNING: Encountered a pragma, but pragmas have yet to be implemented: #pragma {}",
                        out_string
                    );
                    expect_end_of_line = true;
                }
            }
        }
        if expect_end_of_line && !last_directive_was_include {
            // this variable is a bit confusingly named but it is only set after a directive is
            // processed, in which case the preprocessor should not output this line since it
            // would be empty in ALL cases.
            if !line_text.is_empty() {
                return Err(format!("Line text was emitted on a line containing a directive, this should not happen: {}", line_text).into());
            }
            continue;
        }
        current_running_blank_lines = if line_text.trim().is_empty() {
            current_running_blank_lines + 1
        } else {
            0
        };
        if current_running_blank_lines <= 1 {
            out += (line_text + "\n").as_str();
        }
    }

    // println!("DONE INTERPRETING {}", filename);
    Ok((out, context.macros))
}

// 1. find file in either same path as C file or system dir
// 2. lex and interpret that file and steal its list of definitions
// 3. replace this line with the whole preprocessed file (but don't alter
//    line_num).
// 4. concat the define map with this map, with their definitions taking
//    precedence.
fn resolve_include(
    header_filename: &str,
    has_angle_brackets: bool,
    context: &mut InterpreterContext,
) -> Result<String, Box<dyn Error>> {
    if context.include_nesting_count > 200 {
        return Err("Nesting count limit exceeded for #include directive".into());
    }
    let mut file_paths = Vec::new();
    // only check relative filenames if header filename is in ""
    if !has_angle_brackets {
        let mut file_path = context.current_dir.clone();
        file_path.push(header_filename);

        file_paths.push(file_path);
    }

    for mut path in context.default_include_paths.iter().cloned() {
        path.push(header_filename);
        file_paths.push(path);
    }

    for path in file_paths.iter() {
        if fs::exists(path)? {
            let text;
            (text, context.macros) = internal_preprocess(
                path.to_str().ok_or::<Box<dyn Error>>(
                    "Path in include could not be converted to a string".into(),
                )?,
                context.include_nesting_count + 1,
                context.macros.clone(),
            )?;

            // // overwrite entries in the running definition map with the new ones from the included file
            // for (k, v) in new_map {
            //     context.macros.insert(k, v);
            // }

            return Ok(text.trim_end().to_string());
        }
    }

    Err(format!("Could not locate header file {}", header_filename).into())
}

// fn token_is_functional_macro_name(
//     token: PreprocessorToken,
//     context: &mut InterpreterContext,
// ) -> bool {
//     if let PreprocessorToken::Identifier(s) = token {
//         matches!(context.macros.get(&s), Some(Macro::Function(_, _)))
//     } else {
//         false
//     }
// }

fn next_token_is_concatenation(tokens: &VecDeque<PreprocessorToken>) -> bool {
    for token in tokens.iter() {
        match token {
            PreprocessorToken::WhiteSpace(_) => {}
            PreprocessorToken::WidePunctuator(s) if s == "##" => {
                return true;
            }
            _ => return false,
        }
    }
    false
}

fn do_not_prescan(params: &mut Vec<(String, bool)>, name: &str) {
    *params = params
        .iter_mut()
        .map(|(p, should_prescan)| {
            if p == name {
                (p.clone(), false)
            } else {
                (p.clone(), *should_prescan)
            }
        })
        .collect();
}
