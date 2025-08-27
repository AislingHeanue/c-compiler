use std::{
    collections::{HashMap, HashSet, VecDeque},
    error::Error,
    iter::Peekable,
    slice::Iter,
};

use itertools::Itertools;

use crate::preprocessor::lexer::{lex, PreprocessorToken};

use super::{next_token_is_concatenation, InterpreterContext, Macro};

type ResolveReturn<'a, 'b> = (
    Vec<PreprocessorToken>,
    Peekable<Iter<'a, PreprocessorToken>>,
    Peekable<Iter<'b, Vec<PreprocessorToken>>>,
);

pub fn resolve_identifier<'a, 'b: 'a>(
    mut tokens: VecDeque<PreprocessorToken>,
    mut token_iter: Peekable<Iter<'a, PreprocessorToken>>,
    mut line_iter: Peekable<Iter<'b, Vec<PreprocessorToken>>>,
    mut seen_identifiers: HashSet<String>,
    context: &mut InterpreterContext,
) -> Result<ResolveReturn<'a, 'b>, Box<dyn Error>> {
    // println!("resolving {:?}", tokens);
    let mut new_tokens = Vec::new();
    while let Some(token) = tokens.pop_front() {
        let next_is_concatenation = next_token_is_concatenation(&tokens);
        match token {
            PreprocessorToken::Identifier(ref identifier) if !next_is_concatenation => {
                if let Some(m) = context.macros.get(identifier) {
                    if seen_identifiers.contains(identifier) {
                        new_tokens.push(token);
                        continue;
                    }
                    let mut tokens_from_inner = match m.clone() {
                        Macro::Plain(ref inner_tokens) => {
                            seen_identifiers.insert(identifier.to_string());

                            let resolved_inner_tokens;
                            (resolved_inner_tokens, token_iter, line_iter) = resolve_identifier(
                                inner_tokens.clone().into(),
                                token_iter,
                                line_iter,
                                seen_identifiers.clone(),
                                context,
                            )?;
                            seen_identifiers.remove(identifier);

                            resolved_inner_tokens
                        }
                        Macro::Function(params, body) => {
                            // println!("resolved to function with {:?}, {:?}", params, body);
                            let treat_as_function = if !tokens.is_empty() {
                                while !tokens.is_empty()
                                    && matches!(
                                        tokens.front(),
                                        Some(PreprocessorToken::WhiteSpace(_))
                                    )
                                {
                                    tokens.pop_front();
                                }

                                matches!(tokens.front(), Some(PreprocessorToken::Punctuator(s)) if s == "(")
                            } else {
                                while matches!(
                                    token_iter.peek(),
                                    Some(PreprocessorToken::WhiteSpace(_))
                                ) {
                                    token_iter.next();
                                }
                                matches!(token_iter.peek(), Some(PreprocessorToken::Punctuator(s)) if s == "(")
                            };
                            if treat_as_function {
                                if !tokens.is_empty() {
                                    tokens.pop_front();
                                } else {
                                    token_iter.next(); // eat the token we just peeked
                                }

                                // PARSE ARGS
                                let mut paren_nesting = 1;
                                let mut args = Vec::new();
                                let mut this_arg = Vec::new();
                                while paren_nesting != 0 {
                                    let token = if !tokens.is_empty() {
                                        tokens.pop_front().unwrap()
                                    } else if let Some(token) = token_iter.next() {
                                        token.clone()
                                    } else {
                                        let mut found_token = None;
                                        // tokens from this line depleted, use the next line
                                        for line in line_iter.by_ref() {
                                            context.line_num += 1;
                                            token_iter = line.iter().peekable();
                                            // if line isn't empty, pop the token from this line
                                            if let Some(token) = token_iter.next() {
                                                found_token = Some(token.clone());
                                                break;
                                            }
                                        }
                                        if let Some(token) = found_token {
                                            token
                                        } else {
                                            return Err("Unexpected end of file while parsing args for function-like macro".into());
                                        }
                                    };

                                    match token {
                                        PreprocessorToken::Punctuator(ref s) if s == "(" => {
                                            paren_nesting += 1;
                                            this_arg.push(token)
                                        }
                                        PreprocessorToken::Punctuator(ref s) if s == ")" => {
                                            paren_nesting -= 1;
                                            if paren_nesting != 0 {
                                                this_arg.push(token)
                                            } else {
                                                args.push(this_arg);
                                                this_arg = Vec::new();
                                            }
                                        }
                                        PreprocessorToken::Punctuator(s)
                                            if s == "," && paren_nesting == 1 =>
                                        {
                                            args.push(this_arg);
                                            this_arg = Vec::new();
                                        }
                                        PreprocessorToken::WhiteSpace(_) => {
                                            if !this_arg.is_empty() {
                                                this_arg.push(PreprocessorToken::WhiteSpace(
                                                    " ".to_string(),
                                                ));
                                            }
                                        }
                                        _ => this_arg.push(token),
                                    }
                                }
                                if args.len() == 1 && args[0].is_empty() && params.is_empty() {
                                    args = Vec::new();
                                }
                                if args.len() != params.len() {
                                    return Err(
                                        "Incorrect number of args for functional macro invocation"
                                            .into(),
                                    );
                                }
                                // PRESCAN the args before passing them to the function.
                                for (arg, (_, should_prescan)) in args.iter_mut().zip(params.iter())
                                {
                                    if *should_prescan {
                                        (*arg, token_iter, line_iter) = resolve_identifier(
                                            arg.clone().into(),
                                            token_iter,
                                            line_iter,
                                            seen_identifiers.clone(),
                                            context,
                                        )?;
                                    }
                                }

                                let mut arg_map = HashMap::new();
                                for (k, v) in params
                                    .iter()
                                    // ignore should_prescan
                                    .map(|p| p.0.clone())
                                    .zip(args.into_iter())
                                    .collect_vec()
                                {
                                    arg_map.insert(k.clone(), v.clone());
                                }

                                // TEMPLATE ARGS INTO FUNCTION BODY
                                let mut new_body = Vec::new();
                                // println!("{:?}", body);
                                let mut body_iter = body.into_iter();
                                while let Some(body_token) = body_iter.next() {
                                    match body_token {
                                        PreprocessorToken::Identifier(ref s) => {
                                            if let Some(v) = arg_map.get(s) {
                                                new_body.append(&mut v.clone());
                                                continue;
                                            }
                                        }
                                        PreprocessorToken::Punctuator(ref s) if s == "#" => {
                                            let next_token = body_iter.next();
                                            if let Some(PreprocessorToken::Identifier(s)) =
                                                next_token
                                            {
                                                if let Some(v) = arg_map.get(&s) {
                                                    let mut stringified = "".to_string();
                                                    for v_token in v.iter() {
                                                        stringified += v_token.to_string().as_str();
                                                    }
                                                    new_body.push(
                                                        PreprocessorToken::StringLiteral(
                                                            PreprocessorToken::parse_string(
                                                                stringified,
                                                            ),
                                                        ),
                                                    );
                                                    continue;
                                                } else {
                                                    return Err("# in function body must be followed by a function arg".into());
                                                }
                                            } else {
                                                return Err("# in function body must be followed by an identifier".into());
                                            }
                                        }
                                        _ => (),
                                    }
                                    new_body.push(body_token);
                                }
                                // println!("{:?}", new_body);

                                // re-scan the body to expand any further macros
                                seen_identifiers.insert(identifier.to_string());
                                (new_body, token_iter, line_iter) = resolve_identifier(
                                    new_body.into(),
                                    token_iter,
                                    line_iter,
                                    seen_identifiers.clone(),
                                    context,
                                )?;
                                seen_identifiers.remove(identifier);

                                new_body
                            } else {
                                vec![token]
                            }
                        }
                        Macro::Undef => vec![token],
                    };
                    // if the macro resolves to nothing, clear the next whitespace
                    if tokens_from_inner
                        .iter()
                        .all(|t| matches!(t, PreprocessorToken::WhiteSpace(_)))
                    {
                        while matches!(tokens.front(), Some(PreprocessorToken::WhiteSpace(_))) {
                            tokens.pop_front();
                        }
                    } else {
                        new_tokens.append(&mut tokens_from_inner);
                    }
                } else {
                    new_tokens.push(token);
                }
            }
            t if next_is_concatenation => {
                let mut concatenate = next_is_concatenation;
                let mut this_token = t;
                let mut inner_tokens = VecDeque::new();
                while concatenate {
                    let left = this_token.to_string();

                    let mut next = tokens.pop_front();
                    while let Some(PreprocessorToken::WhiteSpace(_)) = next {
                        next = tokens.pop_front();
                    }
                    next = tokens.pop_front(); // pop the ## we've detected
                    while let Some(PreprocessorToken::WhiteSpace(_)) = next {
                        next = tokens.pop_front();
                    }
                    // if next.is_none() {
                    //     return Err("## cannot be at the end of an expression".into());
                    // }
                    let right = next
                        .unwrap_or(PreprocessorToken::WhiteSpace("".to_string()))
                        .to_string();

                    concatenate = next_token_is_concatenation(&tokens);

                    let new_s = format!("{}{}", left, right);
                    let lexed = lex(vec![new_s])?;
                    if lexed.0.len() != 1 {
                        unreachable!()
                    }
                    if lexed.0[0].len() == 1 {
                        this_token = lexed.0[0][0].clone()
                    } else if lexed.0.len() == 2 {
                        println!(
                            "WARNING: {} could not be resolved to a single token, returning both",
                            lexed
                        );
                        inner_tokens.push_back(lexed.0[0][0].clone());
                        this_token = lexed.0[0][1].clone();
                    } else {
                        unreachable!()
                    }
                }
                inner_tokens.push_back(this_token);

                // seen_identifiers.insert(identifier.to_string());

                let mut resolved_inner_tokens;
                (resolved_inner_tokens, token_iter, line_iter) = resolve_identifier(
                    inner_tokens,
                    token_iter,
                    line_iter,
                    seen_identifiers.clone(),
                    context,
                )?;
                // seen_identifiers.remove(identifier);

                // ignore any whitespace after concatenations that only result in whitespace
                if resolved_inner_tokens
                    .iter()
                    .all(|t| matches!(t, PreprocessorToken::WhiteSpace(_)))
                {
                    while matches!(tokens.front(), Some(PreprocessorToken::WhiteSpace(_))) {
                        tokens.pop_front();
                    }
                } else {
                    new_tokens.append(&mut resolved_inner_tokens);
                }
            }
            _ => {
                new_tokens.push(token);
            }
        }
    }

    Ok((new_tokens, token_iter, line_iter))
}
