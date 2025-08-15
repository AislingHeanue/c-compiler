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
    UnsignedInteger,
    UnsignedLong,
    Double,
    // return type, param types
    Function(Box<Type>, Vec<Type>),
    // type and length
    Array(Box<Type>, u64),
    Pointer(Box<Type>),
    Char,
    SignedChar,
    UnsignedChar,
    Void,
    // struct name, is_union
    Struct(String, bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
    Double(f64),
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

#[derive(Debug, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StaticInitialiser {
    Comparable(ComparableStatic),
    Double(f64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComparableStatic {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
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
