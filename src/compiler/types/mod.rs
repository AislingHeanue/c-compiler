use std::collections::HashMap;

use super::parser::StructInfo;

mod constant;
mod parsed_types;
mod static_initialiser;
mod struct_member;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Long,
    LongLong,
    Short,
    UnsignedInteger,
    UnsignedLong,
    UnsignedLongLong,
    UnsignedShort,
    Float,
    Double,
    LongDouble,
    // return type, param types
    Function(Box<Type>, Vec<Type>),
    // type and length
    Array(Box<Type>, Option<u64>),
    // bool denotes whether the pointer is marked as restricted
    Pointer(Box<Type>, bool),
    Char,
    SignedChar,
    UnsignedChar,
    Void,
    // struct name, is_union
    Struct(String, bool),
}

#[derive(Clone, Debug)]
pub enum Constant {
    Integer(i32),
    Long(i64),
    LongLong(i64),
    Short(i16),
    UnsignedInteger(u32),
    UnsignedLong(u64),
    UnsignedLongLong(u64),
    UnsignedShort(u16),
    Float(f32),
    Double(f64),
    LongDouble(f64),
    Char(i8),
    UnsignedChar(u8),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInitialiser>),
    None,
}

#[derive(Debug, Clone)]
pub enum StorageInfo {
    // is_defined and global (ie non-static)
    Function(bool, bool),
    // initializer and global
    Static(InitialValue, bool),
    Constant(StaticInitialiser),
    Automatic,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone)]
pub enum StaticInitialiser {
    Comparable(ComparableStatic),
    Float(f32),
    Double(f64),
    LongDouble(f64),
    FunctionPointer(String),
}

fn one_double_is_negative_zero(a: f64, b: f64) -> bool {
    let a_n0 = a.to_bits() == (-0.0_f64).to_bits();
    let b_n0 = b.to_bits() == (-0.0_f64).to_bits();
    (a_n0 && !b_n0) || (b_n0 && !a_n0)
}

fn one_float_is_negative_zero(a: f32, b: f32) -> bool {
    let a_n0 = a.to_bits() == (-0.0_f32).to_bits();
    let b_n0 = b.to_bits() == (-0.0_f32).to_bits();
    (a_n0 && !b_n0) || (b_n0 && !a_n0)
}

impl PartialEq for StaticInitialiser {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Comparable(a), Self::Comparable(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => {
                // 0.0 does not equal -0.0 !!!
                if one_float_is_negative_zero(*a, *b) {
                    false
                } else if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            (Self::Double(a), Self::Double(b)) => {
                // 0.0 does not equal -0.0 !!!
                if one_double_is_negative_zero(*a, *b) {
                    false
                } else if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComparableStatic {
    Integer(i32),
    Long(i64),
    LongLong(i64),
    Short(i16),
    UnsignedInteger(u32),
    UnsignedLong(u64),
    UnsignedLongLong(u64),
    UnsignedShort(u16),
    Char(i8),
    UnsignedChar(u8),
    // initialiser representing n * 0x00 bytes
    ZeroBytes(u64),
    String(Vec<i8>, bool),
    Pointer(String),
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub symbol_type: Type,
    pub storage: StorageInfo,
    pub constant: bool,
    pub volatile: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Class {
    Memory,
    Sse,
    Integer,
}

#[derive(Debug, Clone)]
pub struct MemberEntry {
    pub member_type: Type,
    pub name: Option<String>,
    pub offset: u64,
}

pub trait FindMemberName {
    fn find_name(
        &self,
        name: &str,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Option<(MemberEntry, usize)>;
}

pub trait Count {
    fn count(&self, self_is_union: bool, structs: &mut HashMap<String, StructInfo>) -> usize;
}

pub trait Flatten {
    type Output;
    fn flatten(
        &self,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Self::Output;
}

pub trait FlattenAndExpandUnions {
    type Output;
    fn flatten_and_expand_unions(
        &self,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Self::Output;
}
