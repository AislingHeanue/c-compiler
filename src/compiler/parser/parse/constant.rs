use std::{collections::HashMap, error::Error};

use crate::compiler::{
    parser::{BinaryOperatorNode, ExpressionWithoutType, UnaryOperatorNode},
    types::{Constant, Type},
};

impl ExpressionWithoutType {
    pub fn fold_to_constant(&self) -> Result<Constant, Box<dyn Error>> {
        let c = match self {
            ExpressionWithoutType::Constant(c) => {
                if matches!(
                    c,
                    Constant::Float(_) | Constant::Double(_) | Constant::LongDouble(_)
                ) {
                    return Err("Constant expression may not include a floating point value".into());
                }

                c.clone()
            }
            ExpressionWithoutType::Unary(op, e) => {
                let c = e.0.fold_to_constant()?;
                match &op {
                    UnaryOperatorNode::Complement => c.complement(),
                    UnaryOperatorNode::Negate => -c,
                    UnaryOperatorNode::Identity => c,
                    UnaryOperatorNode::Not => !c,
                    UnaryOperatorNode::PrefixIncrement => {
                        return Err("Cannot use ++ on a constant value".into())
                    }
                    UnaryOperatorNode::PrefixDecrement => {
                        return Err("Cannot use -- on a constant value".into())
                    }
                    UnaryOperatorNode::SuffixIncrement => {
                        return Err("Cannot use ++ on a constant value".into())
                    }
                    UnaryOperatorNode::SuffixDecrement => {
                        return Err("Cannot use -- on a constant value".into())
                    }
                }
            }
            ExpressionWithoutType::Binary(op, left, right) => {
                let mut left = left.0.fold_to_constant()?;
                let mut right = right.0.fold_to_constant()?;

                let original_left = left.clone();
                let original_right = right.clone();
                Constant::convert_to_common_type(&mut left, &mut right);

                match op {
                    BinaryOperatorNode::Add => left + right,
                    BinaryOperatorNode::Subtract => left - right,
                    BinaryOperatorNode::Multiply => left * right,
                    BinaryOperatorNode::Divide => left / right,
                    BinaryOperatorNode::Mod => left % right,
                    BinaryOperatorNode::And => Constant::Integer(
                        (!original_left.is_zero() && !original_right.is_zero()) as i32,
                    ),
                    BinaryOperatorNode::Or => Constant::Integer(
                        (!original_left.is_zero() || !original_right.is_zero()) as i32,
                    ),
                    BinaryOperatorNode::Equal => Constant::Integer((left == right) as i32),
                    BinaryOperatorNode::NotEqual => Constant::Integer((left != right) as i32),
                    BinaryOperatorNode::Less => Constant::Integer((left < right) as i32),
                    BinaryOperatorNode::Greater => Constant::Integer((left > right) as i32),
                    BinaryOperatorNode::LessEqual => Constant::Integer((left <= right) as i32),
                    BinaryOperatorNode::GreaterEqual => Constant::Integer((left >= right) as i32),
                    BinaryOperatorNode::BitwiseAnd => left & right,
                    BinaryOperatorNode::BitwiseXor => left ^ right,
                    BinaryOperatorNode::BitwiseOr => left | right,
                    BinaryOperatorNode::ShiftLeft => original_left << original_right,
                    BinaryOperatorNode::ShiftRight => original_left >> original_right,
                }
            }
            ExpressionWithoutType::Compound(_, _, _, _) => {
                return Err("Cannot use compound assignment in a constant".into())
            }
            ExpressionWithoutType::Subscript(_, _) => {
                return Err("Cannot use subscript in a constant".into())
            }
            ExpressionWithoutType::Var(_) => {
                return Err("Cannot use variables in a constant".into())
            }
            ExpressionWithoutType::Assignment(_, _) => {
                return Err("Cannot use assignment in a constant".into())
            }

            ExpressionWithoutType::Ternary(cond, left, right) => {
                let cond = cond.0.fold_to_constant()?;
                if cond.is_zero() {
                    right.0.fold_to_constant()?
                } else {
                    left.0.fold_to_constant()?
                }
            }

            ExpressionWithoutType::FunctionCall(_, _) => {
                return Err("Cannot use a function call in a constant".into())
            }

            ExpressionWithoutType::IndirectFunctionCall(_, _) => {
                return Err("Cannot use a function call in a constant".into())
            }
            ExpressionWithoutType::BuiltinFunctionCall(_) => {
                return Err("Cannot use a builtin function call in a constant".into())
            }
            ExpressionWithoutType::Cast(t, e, _) => {
                if !t.is_integer() {
                    return Err("Cannot cast to a non-integer type in a constant".into());
                }
                let e = e.0.fold_to_constant()?;
                e.convert_to(t)
            }

            ExpressionWithoutType::Dereference(_) => {
                return Err("Cannot dereference in a constant".into())
            }

            ExpressionWithoutType::AddressOf(_) => {
                // not strictly following the spec here, taking addresses of static variables may
                // be allowed in some contexts
                return Err("Cannot use take address of another variable in a constant".into());
            }

            ExpressionWithoutType::String(_) => {
                return Err("Cannot use a string in a constant".into())
            }

            ExpressionWithoutType::SizeOf(_) => {
                return Err("Cannot use sizeof of an expression in a constant".into())
            }

            ExpressionWithoutType::SizeOfType(t, _) => {
                // this should definitely be possible according to the spec
                if matches!(t, Type::Struct(_, _)) {
                    return Err("Cannot get the size of a struct in a constant".into());
                }
                Constant::UnsignedInteger(t.get_size(&mut HashMap::new()) as u32)
            }
            ExpressionWithoutType::Dot(_, _) => return Err("Cannot use '.' in a constant".into()),

            ExpressionWithoutType::Arrow(_, _) => {
                return Err("Cannot use '->' in a constant".into())
            }
        };
        Ok(c)
    }
}
