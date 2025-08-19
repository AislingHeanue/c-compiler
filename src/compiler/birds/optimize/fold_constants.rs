use std::error::Error;

use crate::compiler::{birds::BirdsUnaryOperatorNode, types::Constant};

use super::{BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsValueNode, OptimizeContext};

pub trait FoldConstants {
    fn fold_constants(&mut self, context: &mut OptimizeContext) -> Result<(), Box<dyn Error>>;
}

impl FoldConstants for Vec<BirdsInstructionNode> {
    fn fold_constants(&mut self, context: &mut OptimizeContext) -> Result<(), Box<dyn Error>> {
        *self = self
            .drain(..)
            .filter_map(|i| i.fold_constants(context))
            .collect();
        Ok(())
    }
}

impl BirdsInstructionNode {
    fn fold_constants(self, context: &mut OptimizeContext) -> Option<BirdsInstructionNode> {
        match self {
            BirdsInstructionNode::Copy(
                BirdsValueNode::Constant(ref c),
                BirdsValueNode::Var(ref name),
            ) => {
                let t = context.symbols.get(name).unwrap().symbol_type.clone();
                if t.is_integer() {
                    Some(BirdsInstructionNode::Copy(
                        BirdsValueNode::Constant(c.convert_to(&t)),
                        BirdsValueNode::Var(name.clone()),
                    ))
                } else {
                    Some(self)
                }
            }
            BirdsInstructionNode::Unary(op, BirdsValueNode::Constant(src), dst) => {
                Some(BirdsInstructionNode::Copy(
                    match op {
                        BirdsUnaryOperatorNode::Complement => {
                            BirdsValueNode::Constant(src.complement())
                        }
                        BirdsUnaryOperatorNode::Negate => BirdsValueNode::Constant(-src),
                        BirdsUnaryOperatorNode::Not => BirdsValueNode::Constant(!src),
                    },
                    dst,
                ))
            }
            BirdsInstructionNode::Binary(
                op,
                BirdsValueNode::Constant(left),
                BirdsValueNode::Constant(right),
                dst,
            ) => Some(BirdsInstructionNode::Copy(
                match op {
                    BirdsBinaryOperatorNode::Add => BirdsValueNode::Constant(left + right),
                    BirdsBinaryOperatorNode::Subtract => BirdsValueNode::Constant(left - right),
                    BirdsBinaryOperatorNode::Multiply => BirdsValueNode::Constant(left * right),
                    BirdsBinaryOperatorNode::Divide => BirdsValueNode::Constant(left / right),
                    BirdsBinaryOperatorNode::Mod => BirdsValueNode::Constant(left % right),
                    BirdsBinaryOperatorNode::Equal => {
                        BirdsValueNode::Constant(Constant::Integer((left.double_eq(&right)) as i32))
                    }
                    BirdsBinaryOperatorNode::NotEqual => BirdsValueNode::Constant(
                        Constant::Integer(!(left.double_eq(&right)) as i32),
                    ),
                    BirdsBinaryOperatorNode::Less => {
                        BirdsValueNode::Constant(Constant::Integer((left < right) as i32))
                    }
                    BirdsBinaryOperatorNode::Greater => {
                        BirdsValueNode::Constant(Constant::Integer((left > right) as i32))
                    }
                    BirdsBinaryOperatorNode::LessEqual => {
                        BirdsValueNode::Constant(Constant::Integer((left <= right) as i32))
                    }
                    BirdsBinaryOperatorNode::GreaterEqual => {
                        BirdsValueNode::Constant(Constant::Integer((left >= right) as i32))
                    }
                    BirdsBinaryOperatorNode::BitwiseAnd => BirdsValueNode::Constant(left & right),
                    BirdsBinaryOperatorNode::BitwiseXor => BirdsValueNode::Constant(left ^ right),
                    BirdsBinaryOperatorNode::BitwiseOr => BirdsValueNode::Constant(left | right),
                    BirdsBinaryOperatorNode::ShiftLeft => BirdsValueNode::Constant(left << right),
                    BirdsBinaryOperatorNode::ShiftRight => BirdsValueNode::Constant(left >> right),
                },
                dst,
            )),
            BirdsInstructionNode::JumpZero(BirdsValueNode::Constant(ref c), ref label) => {
                if c.is_zero() {
                    Some(BirdsInstructionNode::Jump(label.to_string()))
                } else {
                    None
                }
            }
            BirdsInstructionNode::JumpNotZero(BirdsValueNode::Constant(c), label) => {
                if c.is_zero() {
                    None
                } else {
                    Some(BirdsInstructionNode::Jump(label.to_string()))
                }
            }
            BirdsInstructionNode::JumpCondition(
                op,
                BirdsValueNode::Constant(left),
                BirdsValueNode::Constant(right),
                label,
            ) => match op {
                BirdsBinaryOperatorNode::Equal => {
                    if left.double_eq(&right) {
                        Some(BirdsInstructionNode::Jump(label))
                    } else {
                        None
                    }
                }
                BirdsBinaryOperatorNode::NotEqual => {
                    if !left.double_eq(&right) {
                        Some(BirdsInstructionNode::Jump(label))
                    } else {
                        None
                    }
                }
                BirdsBinaryOperatorNode::Less => {
                    if left < right {
                        Some(BirdsInstructionNode::Jump(label))
                    } else {
                        None
                    }
                }
                BirdsBinaryOperatorNode::Greater => {
                    if left > right {
                        Some(BirdsInstructionNode::Jump(label))
                    } else {
                        None
                    }
                }
                BirdsBinaryOperatorNode::LessEqual => {
                    if left <= right {
                        Some(BirdsInstructionNode::Jump(label))
                    } else {
                        None
                    }
                }
                BirdsBinaryOperatorNode::GreaterEqual => {
                    if left >= right {
                        Some(BirdsInstructionNode::Jump(label))
                    } else {
                        None
                    }
                }
                t => panic!("{:?}", t),
            },
            BirdsInstructionNode::Truncate(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            BirdsInstructionNode::SignedExtend(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            BirdsInstructionNode::ZeroExtend(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            BirdsInstructionNode::IntToDouble(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            BirdsInstructionNode::UintToDouble(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            BirdsInstructionNode::DoubleToInt(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            BirdsInstructionNode::DoubleToUint(
                BirdsValueNode::Constant(c),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_type = context.symbols.get(&dst_name).unwrap().symbol_type.clone();
                Some(BirdsInstructionNode::Copy(
                    BirdsValueNode::Constant(c.convert_to(&dst_type)),
                    BirdsValueNode::Var(dst_name),
                ))
            }
            _ => Some(self),
        }
    }
}
