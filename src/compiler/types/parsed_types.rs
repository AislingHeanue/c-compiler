use std::collections::HashMap;

use crate::compiler::{codegen::align_stack_size, parser::StructInfo};

use super::{Class, MemberEntry, Type};

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
            Type::LongLong => 8,
            Type::Float => 4,
            Type::Double => 8,
            Type::LongDouble => 8,
            Type::UnsignedInteger => 4,
            Type::UnsignedLong => 8,
            Type::UnsignedShort => 2,
            Type::UnsignedLongLong => 8,
            Type::Pointer(_, _) => 8, // pointer is stored like u64
            Type::Char => 1,
            Type::SignedChar => 1,
            Type::UnsignedChar => 1,
            Type::Function(_, _, _) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Array(t, size) => {
                align_stack_size(t.get_size(structs), t.get_alignment(structs)) * (size.unwrap())
            } // arrays are like pointers except that they aren't
            Type::Struct(name, _) => structs.get(name).unwrap().size,
            Type::Enum(_) => 4,
        }
    }

    // this ensures that LongLong is treated as the common type in any arithmetic expression
    // involving long. This is needed because LongLong outranks long despite being the same size
    pub fn get_size_for_common_type(&self, structs: &mut HashMap<String, StructInfo>) -> f32 {
        let mut result = self.get_size(structs) as f32;
        if matches!(self, Type::LongLong | Type::UnsignedLongLong) {
            result += 0.1;
        }
        result
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Type::Integer
                | Type::Long
                | Type::Float
                | Type::Double
                | Type::LongDouble
                | Type::Char
                | Type::SignedChar
                | Type::Short
                | Type::LongLong
                | Type::Enum(_)
        )
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Type::Integer
                | Type::Long
                | Type::LongLong
                | Type::Short
                | Type::UnsignedInteger
                | Type::UnsignedLong
                | Type::UnsignedLongLong
                | Type::UnsignedShort
                | Type::Float
                | Type::Double
                | Type::LongDouble
                | Type::Char
                | Type::SignedChar
                | Type::UnsignedChar
                | Type::Enum(_)
        )
    }

    pub fn is_integer(&self) -> bool {
        !self.is_float() && self.is_arithmetic()
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float | Type::Double | Type::LongDouble)
    }

    pub fn is_scalar(&self) -> bool {
        !matches!(
            self,
            Type::Array(..) | Type::Void | Type::Function(_, _, _) | Type::Struct(_, _)
        )
    }

    pub fn is_complete(&self, structs: &mut HashMap<String, StructInfo>) -> bool {
        match self {
            Type::Struct(name, _) => structs.contains_key(name),
            Type::Void => false,
            Type::Function(_, _, _) => false,
            Type::Array(_, s) => s.is_some(),
            Type::Enum(members) => members.is_some(),
            _ => true,
        }
    }

    pub fn is_complete_pointer(&self, structs: &mut HashMap<String, StructInfo>) -> bool {
        if let Type::Pointer(p1, _) = self {
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

    pub fn promote_for_variadic(&self) -> &Type {
        match self {
            Type::Char => &Type::Integer,
            Type::SignedChar => &Type::Integer,
            Type::UnsignedChar => &Type::Integer,
            Type::Short => &Type::Integer,
            Type::UnsignedShort => &Type::Integer,
            Type::Float => &Type::Double,
            _ => self,
        }
    }

    pub fn class(&self) -> Class {
        match self {
            Type::Float => Class::Sse,
            Type::Double => Class::Sse,
            Type::LongDouble => Class::Sse,
            Type::Array(..) => unreachable!(),
            Type::Function(_, _, _) => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
            Type::Void => unreachable!(),
            _ => Class::Integer,
        }
    }

    pub fn equal_for_assignment(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Pointer(a, _), Type::Pointer(b, _)) => a == b,
            _ => self == other,
        }
    }

    pub fn is_va_list(&self, structs: &mut HashMap<String, StructInfo>) -> bool {
        if let Type::Array(b, Some(1)) = self {
            if let Type::Struct(ref s, false) = **b {
                if let Some(info) = structs.get(s) {
                    info.members
                        == vec![
                            MemberEntry {
                                member_type: Type::UnsignedInteger,
                                name: Some("gp_offset".to_string()),
                                offset: 0,
                            },
                            MemberEntry {
                                member_type: Type::UnsignedInteger,
                                name: Some("fp_offset".to_string()),
                                offset: 4,
                            },
                            MemberEntry {
                                member_type: Type::Pointer(Box::new(Type::Void), false),
                                name: Some("overflow_arg_area".to_string()),
                                offset: 8,
                            },
                            MemberEntry {
                                member_type: Type::Pointer(Box::new(Type::Void), false),
                                name: Some("reg_save_area".to_string()),
                                offset: 16,
                            },
                        ]
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}
