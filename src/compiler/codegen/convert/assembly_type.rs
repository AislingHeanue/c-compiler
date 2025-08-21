use crate::compiler::{
    birds::BirdsValueNode,
    types::{Constant, Type},
};
use std::error::Error;

use super::{AssemblyType, Convert, ConvertContext};

impl Convert<AssemblyType> for Type {
    fn convert(self, context: &mut ConvertContext) -> Result<AssemblyType, Box<dyn Error>> {
        match self {
            Type::Integer => Ok(AssemblyType::Longword),
            Type::Long => Ok(AssemblyType::Quadword),
            Type::Short => Ok(AssemblyType::Word),
            Type::UnsignedInteger => Ok(AssemblyType::Longword),
            Type::UnsignedLong => Ok(AssemblyType::Quadword),
            Type::UnsignedShort => Ok(AssemblyType::Word),
            Type::Pointer(_) => Ok(AssemblyType::Quadword),
            Type::Double => Ok(AssemblyType::Double),
            Type::Array(t, size) => {
                let assembly_t = (*t).convert(context)?;
                let size = assembly_t.get_size() * size;
                let alignment = if size < 16 {
                    assembly_t.get_alignment()
                } else {
                    16
                };
                Ok(AssemblyType::ByteArray(size, alignment))
            }
            Type::Function(_, _) => Err("Tried to convert a function type".into()),
            Type::Char | Type::SignedChar | Type::UnsignedChar => Ok(AssemblyType::Byte),
            Type::Struct(name, _) => {
                let info = context.structs.get(&name).unwrap().clone();
                Ok(AssemblyType::ByteArray(info.size, info.alignment as u32))
            }
            Type::Void => unreachable!(),
        }
    }
}

impl AssemblyType {
    pub fn infer(
        src: &BirdsValueNode,
        context: &mut ConvertContext,
    ) -> Result<(AssemblyType, bool, bool), Box<dyn Error>> {
        // returns the assembly type and whether the value is a signed number and a scalar
        Ok(match src {
            BirdsValueNode::Constant(Constant::Integer(_)) => (AssemblyType::Longword, true, true),
            BirdsValueNode::Constant(Constant::Long(_)) => (AssemblyType::Quadword, true, true),
            BirdsValueNode::Constant(Constant::UnsignedInteger(_)) => {
                (AssemblyType::Longword, false, true)
            }
            BirdsValueNode::Constant(Constant::UnsignedLong(_)) => {
                (AssemblyType::Quadword, false, true)
            }
            BirdsValueNode::Constant(Constant::Double(_)) => (AssemblyType::Double, true, true),
            BirdsValueNode::Constant(Constant::Char(_)) => (AssemblyType::Byte, true, true),
            BirdsValueNode::Constant(Constant::UnsignedChar(_)) => {
                (AssemblyType::Byte, false, true)
            }
            BirdsValueNode::Constant(Constant::Short(_)) => (AssemblyType::Word, true, true),
            BirdsValueNode::Constant(Constant::UnsignedShort(_)) => {
                (AssemblyType::Word, false, true)
            }
            BirdsValueNode::Var(name) => {
                let var_type = context.symbols.get(name).unwrap().symbol_type.clone();
                let signed = var_type.is_signed();
                let scalar = var_type.is_scalar();
                (var_type.convert(context)?, signed, scalar)
            }
        })
    }

    pub fn get_eightbyte(offset: i32, total_size: i32) -> AssemblyType {
        match total_size - offset {
            8.. => AssemblyType::Quadword,
            4 => AssemblyType::Longword,
            1 => AssemblyType::Byte,
            i => AssemblyType::ByteArray(i as u64, 8),
        }
    }

    pub fn get_alignment(&self) -> u32 {
        match self {
            AssemblyType::Byte => 1,
            AssemblyType::Word => 2,
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(_size, alignment) => *alignment,
        }
    }

    pub fn get_size(&self) -> u64 {
        match self {
            AssemblyType::Byte => 1,
            AssemblyType::Word => 2,
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(size, _alignment) => *size,
        }
    }
}
