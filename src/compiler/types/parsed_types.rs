use std::collections::HashMap;

use crate::compiler::{codegen::align_stack_size, parser::StructInfo};

use super::Type;

impl Type {
    pub fn get_alignment(&self, structs: &mut HashMap<String, StructInfo>) -> u64 {
        // bytes
        match self {
            Type::Struct(name) => structs.get(name).unwrap().alignment,
            Type::Array(inner, _) => inner.get_alignment(structs),
            _ => self.get_size(structs),
        }
    }

    pub fn get_size(&self, structs: &mut HashMap<String, StructInfo>) -> u64 {
        // bytes
        match self {
            Type::Integer => 4,
            Type::Long => 8,
            Type::Double => 8,
            Type::UnsignedInteger => 4,
            Type::UnsignedLong => 8,
            Type::Pointer(_) => 8, // pointer is stored like u64
            Type::Char => 1,
            Type::SignedChar => 1,
            Type::UnsignedChar => 1,
            Type::Function(_, _) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Array(t, size) => {
                align_stack_size(t.get_size(structs), t.get_alignment(structs)) * (*size)
            } // arrays are like pointers except that they aren't
            Type::Struct(name) => structs.get(name).unwrap().size,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Type::Integer => true,
            Type::Long => true,
            Type::UnsignedInteger => false,
            Type::UnsignedLong => false,
            Type::Double => true,
            Type::Pointer(_) => false,
            Type::Array(..) => false,
            Type::Function(_, _) => unreachable!(),
            Type::Char => true,
            Type::SignedChar => true,
            Type::UnsignedChar => false,
            Type::Void => unreachable!(),
            Type::Struct(_) => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            Type::Integer => true,
            Type::Long => true,
            Type::UnsignedInteger => true,
            Type::UnsignedLong => true,
            Type::Double => true,
            Type::Pointer(_) => false,
            Type::Function(_, _) => false,
            Type::Array(..) => false,
            Type::Char => true,
            Type::SignedChar => true,
            Type::UnsignedChar => true,
            Type::Void => false,
            Type::Struct(_) => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Double => false,
            _ => self.is_arithmetic(),
        }
    }

    pub fn is_scalar(&self) -> bool {
        !matches!(
            self,
            Type::Array(..) | Type::Void | Type::Function(_, _) | Type::Struct(_)
        )
    }

    pub fn is_complete(&self, structs: &mut HashMap<String, StructInfo>) -> bool {
        match self {
            Type::Struct(name) => structs.contains_key(name),
            Type::Void => false,
            _ => true,
        }
    }

    pub fn is_complete_pointer(&self, structs: &mut HashMap<String, StructInfo>) -> bool {
        if let Type::Pointer(p1) = self {
            p1.is_complete(structs)
        } else {
            false
        }
    }

    pub fn is_character(&self) -> bool {
        matches!(self, Type::Char | Type::SignedChar | Type::UnsignedChar)
    }

    pub fn promote(&self) -> &Type {
        match self {
            Type::Char => &Type::Integer,
            Type::SignedChar => &Type::Integer,
            Type::UnsignedChar => &Type::Integer,
            _ => self,
        }
    }
}
