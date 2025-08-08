use std::error::Error;

use super::{BinaryOperator, ConditionCode, Convert, ConvertContext, UnaryOperator};
use crate::compiler::birds::{BirdsBinaryOperatorNode, BirdsUnaryOperatorNode};

impl Convert<UnaryOperator> for BirdsUnaryOperatorNode {
    fn convert(self, _context: &mut ConvertContext) -> Result<UnaryOperator, Box<dyn Error>> {
        match self {
            BirdsUnaryOperatorNode::Negate => Ok(UnaryOperator::Neg),
            BirdsUnaryOperatorNode::Complement => Ok(UnaryOperator::Not),
            BirdsUnaryOperatorNode::Not => {
                panic!("Should not directly convert ! to a unary operator")
            }
        }
    }
}

impl Convert<BinaryOperator> for BirdsBinaryOperatorNode {
    fn convert(self, _context: &mut ConvertContext) -> Result<BinaryOperator, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::Add => Ok(BinaryOperator::Add),
            BirdsBinaryOperatorNode::Subtract => Ok(BinaryOperator::Sub),
            BirdsBinaryOperatorNode::Multiply => Ok(BinaryOperator::Mult),
            BirdsBinaryOperatorNode::BitwiseAnd => Ok(BinaryOperator::And),
            BirdsBinaryOperatorNode::BitwiseXor => Ok(BinaryOperator::Xor),
            BirdsBinaryOperatorNode::BitwiseOr => Ok(BinaryOperator::Or),
            BirdsBinaryOperatorNode::ShiftLeft => Ok(BinaryOperator::ShiftLeft),
            BirdsBinaryOperatorNode::ShiftRight => Ok(BinaryOperator::ShiftRight),
            BirdsBinaryOperatorNode::Divide => Ok(BinaryOperator::DivDouble),
            BirdsBinaryOperatorNode::Mod => {
                panic!("should not treat mod as binary expressions during codegen")
            }
            BirdsBinaryOperatorNode::Equal
            | BirdsBinaryOperatorNode::NotEqual
            | BirdsBinaryOperatorNode::Less
            | BirdsBinaryOperatorNode::Greater
            | BirdsBinaryOperatorNode::LessEqual
            | BirdsBinaryOperatorNode::GreaterEqual => {
                panic!("relational expressions should not be treated as binary expressions")
            }
        }
    }
}

impl BirdsBinaryOperatorNode {
    pub fn convert_unsigned(
        self,
        context: &mut ConvertContext,
    ) -> Result<BinaryOperator, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::ShiftLeft => Ok(BinaryOperator::UnsignedShiftLeft),
            BirdsBinaryOperatorNode::ShiftRight => Ok(BinaryOperator::UnsignedShiftRight),
            _ => self.convert(context),
        }
    }
}

impl Convert<ConditionCode> for BirdsBinaryOperatorNode {
    fn convert(self, _context: &mut ConvertContext) -> Result<ConditionCode, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::Equal => Ok(ConditionCode::E),
            BirdsBinaryOperatorNode::NotEqual => Ok(ConditionCode::Ne),
            BirdsBinaryOperatorNode::Less => Ok(ConditionCode::L),
            BirdsBinaryOperatorNode::Greater => Ok(ConditionCode::G),
            BirdsBinaryOperatorNode::LessEqual => Ok(ConditionCode::Le),
            BirdsBinaryOperatorNode::GreaterEqual => Ok(ConditionCode::Ge),
            _ if !self.is_relational() => {
                panic!("non-relational binary expressions should not be treated as relational expressions")
            }
            _ => unreachable!(),
        }
    }
}

impl BirdsBinaryOperatorNode {
    pub fn convert_condition_unsigned(
        self,
        _context: &mut ConvertContext,
    ) -> Result<ConditionCode, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::Equal => Ok(ConditionCode::E),
            BirdsBinaryOperatorNode::NotEqual => Ok(ConditionCode::Ne),
            BirdsBinaryOperatorNode::Less => Ok(ConditionCode::B),
            BirdsBinaryOperatorNode::Greater => Ok(ConditionCode::A),
            BirdsBinaryOperatorNode::LessEqual => Ok(ConditionCode::Be),
            BirdsBinaryOperatorNode::GreaterEqual => Ok(ConditionCode::Ae),
            _ if !self.is_relational() => {
                panic!("non-relational binary expressions should not be treated as relational expressions")
            }
            _ => unreachable!(),
        }
    }
}
