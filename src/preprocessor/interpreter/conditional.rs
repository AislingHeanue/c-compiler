use std::{
    collections::{HashSet, VecDeque},
    error::Error,
    iter::Peekable,
    slice::Iter,
};

use crate::preprocessor::lexer::PreprocessorToken;

use super::{macros::resolve_identifier, InterpreterContext, Macro};

#[derive(Debug)]
enum ConditionNode {
    Binary(BinaryOp, Box<ConditionNode>, Box<ConditionNode>),
    Unary(UnaryOp, Box<ConditionNode>),
    Number(i64),
    Identifier(String),
    Defined(String),
    Ternary(Box<ConditionNode>, Box<ConditionNode>, Box<ConditionNode>),
}

#[derive(Debug, Clone)]
enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl BinaryOp {
    fn precedence(&self) -> usize {
        match self {
            // note: unary has precedence 55
            BinaryOp::Multiply => 50,
            BinaryOp::Divide => 50,
            BinaryOp::Mod => 50,

            BinaryOp::Add => 45,
            BinaryOp::Subtract => 45,

            BinaryOp::ShiftLeft => 40,
            BinaryOp::ShiftRight => 40,

            BinaryOp::Greater => 35,
            BinaryOp::Less => 35,
            BinaryOp::GreaterEqual => 35,
            BinaryOp::LessEqual => 35,

            BinaryOp::Equal => 30,
            BinaryOp::NotEqual => 30,

            BinaryOp::BitwiseAnd => 25,
            BinaryOp::BitwiseOr => 20,
            BinaryOp::BitwiseXor => 15,

            BinaryOp::And => 10,

            BinaryOp::Or => 5,
            // note: ternary has precedence 3
        }
    }

    fn from(s: &str) -> Result<BinaryOp, Box<dyn Error>> {
        Ok(match s {
            "+" => BinaryOp::Add,
            "-" => BinaryOp::Subtract,
            "*" => BinaryOp::Multiply,
            "/" => BinaryOp::Divide,
            "%" => BinaryOp::Mod,
            "<<" => BinaryOp::ShiftLeft,
            ">>" => BinaryOp::ShiftRight,
            "&&" => BinaryOp::And,
            "||" => BinaryOp::Or,
            "&" => BinaryOp::BitwiseAnd,
            "|" => BinaryOp::BitwiseOr,
            "^" => BinaryOp::BitwiseXor,
            ">" => BinaryOp::Greater,
            "<" => BinaryOp::Less,
            ">=" => BinaryOp::GreaterEqual,
            "<=" => BinaryOp::LessEqual,
            "==" => BinaryOp::Equal,
            "!=" => BinaryOp::NotEqual,
            _ => {
                return Err(format!(
                    "Invalid token used as binary operator in #if directive: {}",
                    s
                )
                .into())
            }
        })
    }
}

#[derive(Debug)]
enum UnaryOp {
    Negate,
    Complement,
    Not,
    Identity,
}

impl UnaryOp {
    fn from(s: &str) -> Result<UnaryOp, Box<dyn Error>> {
        Ok(match s {
            "~" => UnaryOp::Complement,
            "!" => UnaryOp::Not,
            "-" => UnaryOp::Negate,
            "+" => UnaryOp::Identity,
            _ => {
                return Err(format!(
                    "Invalid token used as unary operator in #if directive: {}",
                    s
                )
                .into())
            }
        })
    }
}

pub type IfReturn<'a, 'b> = (
    Vec<PreprocessorToken>,
    Peekable<Iter<'a, PreprocessorToken>>,
    Peekable<Iter<'b, Vec<PreprocessorToken>>>,
);

impl ConditionNode {
    fn evaluate(&self, context: &mut InterpreterContext) -> i64 {
        match self {
            ConditionNode::Binary(op, left, right) => {
                Self::evaluate_binary(op, left, right, context)
            }
            ConditionNode::Unary(op, right) => match op {
                UnaryOp::Negate => -right.evaluate(context),
                UnaryOp::Identity => right.evaluate(context),
                // complement = bitwise not
                UnaryOp::Complement => !right.evaluate(context),
                UnaryOp::Not => (right.evaluate(context) == 0) as i64,
            },
            ConditionNode::Number(n) => *n,
            ConditionNode::Identifier(_s) => {
                // this identifier should have already been macro expanded, so if it's still here
                // we return 0 signifying that it isn't defined.
                0
            }
            ConditionNode::Defined(name) => matches!(
                context.macros.get(name),
                Some(Macro::Function(_, _) | Macro::Plain(_))
            ) as i64,
            ConditionNode::Ternary(cond, left, right) => {
                if cond.evaluate(context) != 0 {
                    left.evaluate(context)
                } else {
                    right.evaluate(context)
                }
            }
        }
    }
    fn evaluate_binary(
        op: &BinaryOp,
        left: &ConditionNode,
        right: &ConditionNode,
        context: &mut InterpreterContext,
    ) -> i64 {
        let left = left.evaluate(context);
        let right = right.evaluate(context);

        match op {
            BinaryOp::Add => left + right,
            BinaryOp::Subtract => left - right,
            BinaryOp::Multiply => left * right,
            BinaryOp::Divide => left / right,
            BinaryOp::Mod => left % right,
            BinaryOp::ShiftLeft => left << right,
            BinaryOp::ShiftRight => left >> right,
            BinaryOp::And => (left != 0 && right != 0) as i64,
            BinaryOp::Or => (left != 0 || right != 0) as i64,
            BinaryOp::BitwiseAnd => left ^ right,
            BinaryOp::BitwiseOr => left ^ right,
            BinaryOp::BitwiseXor => left ^ right,
            BinaryOp::Greater => (left > right) as i64,
            BinaryOp::Less => (left < right) as i64,
            BinaryOp::GreaterEqual => (left >= right) as i64,
            BinaryOp::LessEqual => (left <= right) as i64,
            BinaryOp::Equal => (left == right) as i64,
            BinaryOp::NotEqual => (left != right) as i64,
        }
    }
}

pub fn prepare_tokens_for_if<'a, 'b: 'a>(
    mut token_iter: Peekable<Iter<'a, PreprocessorToken>>,
    mut line_iter: Peekable<Iter<'b, Vec<PreprocessorToken>>>,
    context: &mut InterpreterContext,
) -> Result<IfReturn<'a, 'b>, Box<dyn Error>> {
    let mut if_tokens = Vec::new();
    while let Some(token) = token_iter.next() {
        // simply ignore all whitespace
        if let PreprocessorToken::WhiteSpace(_) = token {
            continue;
        }
        if let PreprocessorToken::KeywordDefined = token {
            if_tokens.push(token.clone());
            // if we encounter a 'defined', DO NOT macro expand the next identifier
            // (after whitespace which we ignore)
            let mut token_after_defined = token_iter.next();
            while let Some(PreprocessorToken::WhiteSpace(_)) = token_after_defined {
                token_after_defined = token_iter.next();
            }
            match token_after_defined {
                Some(PreprocessorToken::Identifier(name)) => {
                    if_tokens.push(PreprocessorToken::Identifier(name.clone()));
                }
                Some(PreprocessorToken::Punctuator(s)) if s == "(" => {
                    let mut token_after_paren = token_iter.next();
                    while let Some(PreprocessorToken::WhiteSpace(_)) = token_after_paren {
                        token_after_paren = token_iter.next();
                    }
                    if let Some(PreprocessorToken::Identifier(name)) = token_after_paren {
                        if_tokens.push(PreprocessorToken::Identifier(name.clone()));
                    } else {
                        return Err("defined must be followed by an identifier".into());
                    }
                    // also kill the ) after this
                    let mut token_after_name = token_iter.next();
                    while let Some(PreprocessorToken::WhiteSpace(_)) = token_after_name {
                        token_after_name = token_iter.next();
                    }
                }
                _ => return Err("defined must be followed by an identifier".into()),
            }
        } else {
            let mut resolved_tokens;
            (resolved_tokens, token_iter, line_iter) = resolve_identifier(
                vec![token.clone()].into(),
                token_iter,
                line_iter,
                HashSet::new(),
                context,
            )?;
            if_tokens.append(&mut resolved_tokens);
        }
    }
    if if_tokens.is_empty() {
        return Err("#if, #elif directives must have an expression to evaluate".into());
    }
    // println!("{:?}", if_tokens);
    Ok((if_tokens, token_iter, line_iter))
}

// used for if statements
pub fn resolve_number(
    mut tokens: Vec<PreprocessorToken>,
    context: &mut InterpreterContext,
) -> Result<i64, Box<dyn Error>> {
    // println!("    parsing {:?}", tokens);
    let expression: ConditionNode = parse_with_level(&mut tokens.drain(..).collect(), 0)?;
    // println!("    and evaluating {:?}", expression);
    let i = expression.evaluate(context);
    // println!("    and got {}", i);
    Ok(i)
}

fn parse_with_level(
    tokens: &mut VecDeque<PreprocessorToken>,
    level: usize,
) -> Result<ConditionNode, Box<dyn Error>> {
    let mut left = parse_unary(tokens)?;
    let mut op = get_next_binary_operator_token(tokens)?;
    let mut operator_precedence = op.clone().map(|o| o.0.precedence());
    let mut next_token;
    while let Some(precedence) = operator_precedence {
        next_token = Some(op.clone().unwrap().1);
        if precedence < level {
            // add the token back to the list since we don't consume it here
            tokens.push_front(next_token.unwrap());
            break;
        }
        left = ConditionNode::Binary(
            op.unwrap().0,
            Box::new(left),
            Box::new(parse_with_level(tokens, precedence + 1)?),
        );
        // now get the next operator after this one
        op = get_next_binary_operator_token(tokens)?;
        operator_precedence = op.clone().map(|o| o.0.precedence());
    }
    // 3 is the precedence of ?, so if level is higher than 3 then don't consume ?
    if level <= 3 {
        next_token = tokens.pop_front();
        if matches!(next_token,Some(PreprocessorToken::Punctuator(ref s)) if s == "?") {
            let middle = parse_with_level(tokens, 0)?;
            next_token = tokens.pop_front();
            if !matches!(next_token, Some(PreprocessorToken::Punctuator(ref s)) if s == ":") {
                return Err(format!(
                    "Expected to find : while parsing ternary expression in if statement but got {:?}",
                    next_token
                )
                    .into());
            }

            let right = parse_with_level(tokens, 3 + 1)?;
            left = ConditionNode::Ternary(Box::new(left), Box::new(middle), Box::new(right));
        } else if let Some(found_next_token) = next_token {
            // add the token back to the list since we don't consume it here
            tokens.push_front(found_next_token);
        }
    }
    Ok(left)
}

fn get_next_binary_operator_token(
    tokens: &mut VecDeque<PreprocessorToken>,
) -> Result<Option<(BinaryOp, PreprocessorToken)>, Box<dyn Error>> {
    let next_token = tokens.pop_front();
    let op = match next_token {
        None => None,
        Some(PreprocessorToken::Punctuator(ref s)) if s == ")" || s == "?" || s == ":" => None,
        Some(PreprocessorToken::Punctuator(ref s)) => Some(BinaryOp::from(s)?),
        Some(PreprocessorToken::WidePunctuator(ref s)) => Some(BinaryOp::from(s)?),
        t => return Err(format!("Invalid binary operator found in #if directive: {:?}", t).into()),
    };
    if let Some(found_op) = op {
        Ok(Some((found_op, next_token.unwrap())))
    } else {
        if let Some(found_next_token) = next_token {
            // add the token back to the list since we don't consume it here
            tokens.push_front(found_next_token);
        }
        Ok(None)
    }
}

fn parse_unary(tokens: &mut VecDeque<PreprocessorToken>) -> Result<ConditionNode, Box<dyn Error>> {
    let op = get_next_unary_operator_token(tokens)?;
    if let Some(found_op) = op {
        Ok(ConditionNode::Unary(
            found_op.0,
            Box::new(parse_unary(tokens)?),
        ))
    } else {
        let mut next_token = tokens.pop_front();
        if matches!(next_token, Some(PreprocessorToken::KeywordDefined)) {
            next_token = tokens.pop_front();
            if let Some(PreprocessorToken::Identifier(s)) = next_token {
                Ok(ConditionNode::Defined(s.clone()))
            } else {
                Err("defined keyword must be followed by an identifier".into())
            }
        } else {
            if let Some(found_next_token) = next_token {
                // add the token back to the list since we don't consume it here
                tokens.push_front(found_next_token);
            }
            parse_primary(tokens)
        }
    }
}

fn get_next_unary_operator_token(
    tokens: &mut VecDeque<PreprocessorToken>,
) -> Result<Option<(UnaryOp, PreprocessorToken)>, Box<dyn Error>> {
    let next_token = tokens.pop_front();
    let op = match next_token {
        // signify that this is a ternary expression by not populating op
        Some(PreprocessorToken::Punctuator(ref s)) if s == "(" || s == ")" => None,
        Some(PreprocessorToken::Punctuator(ref s)) => Some(UnaryOp::from(s)?),
        _ => None,
    };
    if let Some(found_op) = op {
        Ok(Some((found_op, next_token.unwrap())))
    } else {
        if let Some(found_next_token) = next_token {
            // add the token back to the list since we don't consume it here
            tokens.push_front(found_next_token);
        }
        Ok(None)
    }
}

fn parse_primary(
    tokens: &mut VecDeque<PreprocessorToken>,
) -> Result<ConditionNode, Box<dyn Error>> {
    if tokens.is_empty() {
        return Err("Unexpected end of tokens while parsing #if directive".into());
    }
    let node = match tokens.pop_front().unwrap() {
        PreprocessorToken::Identifier(s) => ConditionNode::Identifier(s),
        PreprocessorToken::Punctuator(s) if s == "(" => {
            let expression = parse_with_level(tokens, 0)?;
            if !matches!(tokens.pop_front(), Some(PreprocessorToken::Punctuator(s)) if s == ")") {
                return Err("Unclosed ( in expression in #if directive".into());
            }
            expression
        }
        PreprocessorToken::CharacterConstant(i) => ConditionNode::Number(i.into()),
        PreprocessorToken::Number(i) => {
            ConditionNode::Number(i.trim_end_matches(['l', 'L', 'u', 'U']).parse::<i64>()?)
        }
        t => return Err(format!("Invalid token in expression in #if directive, {}", t).into()),
    };
    Ok(node)
}
