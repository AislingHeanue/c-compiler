use crate::compiler::{
    birds::BirdsValueNode,
    types::{Constant, Type},
};
use std::error::Error;

use super::{AssemblyType, Convert, ConvertContext};

impl Convert<AssemblyType> for Type {
    fn convert(self, _context: &mut ConvertContext) -> Result<AssemblyType, Box<dyn Error>> {
        match self {
            Type::Integer => Ok(AssemblyType::Longword),
            Type::Long => Ok(AssemblyType::Quadword),
            Type::UnsignedInteger => Ok(AssemblyType::Longword),
            Type::UnsignedLong => Ok(AssemblyType::Quadword),
            Type::Double => Ok(AssemblyType::Double),
            Type::Pointer(_) => Ok(AssemblyType::Quadword),
            Type::Array(t, size) => {
                let assembly_t = (*t).convert(_context)?;
                let size = assembly_t.get_size() * size as u32;
                let alignment = if size < 16 {
                    assembly_t.get_alignment()
                } else {
                    16
                };
                Ok(AssemblyType::ByteArray(size, alignment))
            }
            Type::Function(_, _) => Err("Tried to convert a function type".into()),
            Type::Char | Type::SignedChar | Type::UnsignedChar => Ok(AssemblyType::Byte),
            Type::Void => unreachable!(),
        }
    }
}

impl AssemblyType {
    pub fn infer(
        src: &BirdsValueNode,
        context: &mut ConvertContext,
    ) -> Result<(AssemblyType, bool), Box<dyn Error>> {
        // returns the assembly type and whether the value is a signed number
        Ok(match src {
            BirdsValueNode::Constant(Constant::Integer(_)) => (AssemblyType::Longword, true),
            BirdsValueNode::Constant(Constant::Long(_)) => (AssemblyType::Quadword, true),
            BirdsValueNode::Constant(Constant::UnsignedInteger(_)) => {
                (AssemblyType::Longword, false)
            }
            BirdsValueNode::Constant(Constant::UnsignedLong(_)) => (AssemblyType::Quadword, false),
            BirdsValueNode::Constant(Constant::Double(_)) => (AssemblyType::Double, true),
            BirdsValueNode::Constant(Constant::Char(_)) => (AssemblyType::Byte, true),
            BirdsValueNode::Constant(Constant::UnsignedChar(_)) => (AssemblyType::Byte, false),
            BirdsValueNode::Var(name) => {
                let var_type = context.symbols.get(name).unwrap().symbol_type.clone();
                let signed = var_type.is_signed();
                (var_type.convert(context)?, signed)
            }
        })
    }

    pub fn get_alignment(&self) -> u32 {
        match self {
            AssemblyType::Byte => 1,
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(_size, alignment) => *alignment,
        }
    }

    pub fn get_size(&self) -> u32 {
        match self {
            AssemblyType::Byte => 1,
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(size, _alignment) => *size,
        }
    }
}
