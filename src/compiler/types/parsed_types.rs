use std::collections::HashMap;

use crate::compiler::{codegen::align_stack_size, parser::StructInfo};

use super::{Class, Type};

impl Type {
    pub fn get_alignment(&self, structs: &mut HashMap<String, StructInfo>) -> u64 {
        // bytes
        match self {
            Type::Struct(name, _) => structs.get(name).unwrap().alignment,
            Type::Array(inner, _) => inner.get_alignment(structs),
            _ => self.get_size(structs),
        }
    }

    pub fn get_size(&self, structs: &mut HashMap<String, StructInfo>) -> u64 {
        // bytes
        match self {
            Type::Integer => 4,
            Type::Long => 8,
            Type::Short => 2,
            Type::Float => 4,
            Type::Double => 8,
            Type::UnsignedInteger => 4,
            Type::UnsignedLong => 8,
            Type::UnsignedShort => 2,
            Type::Pointer(_) => 8, // pointer is stored like u64
            Type::Char => 1,
            Type::SignedChar => 1,
            Type::UnsignedChar => 1,
            Type::Function(_, _) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Array(t, size) => {
                align_stack_size(t.get_size(structs), t.get_alignment(structs)) * (*size)
            } // arrays are like pointers except that they aren't
            Type::Struct(name, _) => structs.get(name).unwrap().size,
        }
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Type::Integer
                | Type::Long
                | Type::Float
                | Type::Double
                | Type::Char
                | Type::SignedChar
                | Type::Short
        )
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Type::Integer
                | Type::Long
                | Type::Short
                | Type::UnsignedInteger
                | Type::UnsignedLong
                | Type::UnsignedShort
                | Type::Float
                | Type::Double
                | Type::Char
                | Type::SignedChar
                | Type::UnsignedChar
        )
    }

    pub fn is_integer(&self) -> bool {
        !self.is_float() && self.is_arithmetic()
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float | Type::Double)
    }

    pub fn is_scalar(&self) -> bool {
        !matches!(
            self,
            Type::Array(..) | Type::Void | Type::Function(_, _) | Type::Struct(_, _)
        )
    }

    pub fn is_complete(&self, structs: &mut HashMap<String, StructInfo>) -> bool {
        match self {
            Type::Struct(name, _) => structs.contains_key(name),
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

    pub fn is_smaller_than_int(&self) -> bool {
        matches!(
            self,
            Type::Char | Type::SignedChar | Type::UnsignedChar | Type::Short | Type::UnsignedShort
        )
    }

    pub fn promote(&self) -> &Type {
        match self {
            Type::Char => &Type::Integer,
            Type::SignedChar => &Type::Integer,
            Type::UnsignedChar => &Type::Integer,
            Type::Short => &Type::Integer,
            Type::UnsignedShort => &Type::Integer,
            _ => self,
        }
    }

    pub fn class(&self) -> Class {
        match self {
            Type::Double => Class::Sse,
            Type::Float => Class::Sse,
            Type::Array(..) => unreachable!(),
            Type::Function(_, _) => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
            Type::Void => unreachable!(),
            _ => Class::Integer,
        }
    }
}
