use crate::compiler::{
    birds::BirdsValueNode,
    types::{Constant, StaticInitialiser},
};
use std::error::Error;

use super::{create_static_constant, Convert, ConvertContext, ImmediateValue, Operand};

impl Convert<Operand> for BirdsValueNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Operand, Box<dyn Error>> {
        match self {
            BirdsValueNode::Constant(Constant::Integer(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c as i64)))
            }
            BirdsValueNode::Constant(Constant::Long(c) | Constant::LongLong(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c)))
            }
            BirdsValueNode::Constant(Constant::UnsignedInteger(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c as u64)))
            }
            BirdsValueNode::Constant(Constant::UnsignedLong(c) | Constant::UnsignedLongLong(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c)))
            }
            BirdsValueNode::Constant(Constant::Char(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c as i64)))
            }
            BirdsValueNode::Constant(Constant::UnsignedChar(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c as u64)))
            }
            BirdsValueNode::Constant(Constant::Short(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c as i64)))
            }
            BirdsValueNode::Constant(Constant::UnsignedShort(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c as u64)))
            }
            BirdsValueNode::Constant(Constant::Float(c)) => Ok(create_static_constant(
                4,
                StaticInitialiser::Float(c),
                context,
            )),
            BirdsValueNode::Constant(Constant::Double(c)) => Ok(create_static_constant(
                8,
                StaticInitialiser::Double(c),
                context,
            )),
            BirdsValueNode::Constant(Constant::LongDouble(c)) => Ok(create_static_constant(
                8,
                StaticInitialiser::LongDouble(c),
                context,
            )),
            BirdsValueNode::Constant(Constant::AddressOf(_)) => unreachable!(),
            BirdsValueNode::Var(s) => {
                let var_type = &context.symbols.get(&s).unwrap().symbol_type;
                if var_type.is_scalar() {
                    Ok(Operand::MockReg(s))
                } else {
                    Ok(Operand::MockMemory(s, 0))
                }
            }
        }
    }
}
