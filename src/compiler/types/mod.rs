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
}

impl Type {
    pub fn get_size(&self) -> i32 {
        // bytes
        match self {
            Type::Integer => 4,
            Type::Long => 8,
            Type::Double => 8,
            Type::UnsignedInteger => 4,
            Type::UnsignedLong => 8,
            Type::Pointer(_) => 8, // pointer is stored like u64
            Type::Array(t, size) => t.get_size() * (*size) as i32, // arrays are like pointers except that they aren't
            Type::Function(_, _) => unreachable!(),
            Type::Char => 1,
            Type::SignedChar => 1,
            Type::UnsignedChar => 1,
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
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Double => false,
            _ => self.is_arithmetic(),
        }
    }

    pub fn is_scalar(&self) -> bool {
        !matches!(self, Type::Array(..))
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

#[derive(Debug, Clone)]
pub enum StorageInfo {
    // is_defined and global (ie non-static)
    Function(bool, bool),
    // initializer and global
    Static(InitialValue, bool),
    Constant(StaticInitial),
    Automatic,
}

#[derive(Debug, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StaticInitial {
    Ordinal(OrdinalStatic),
    Double(f64),
    // initialiser representing n * 0x00 bytes
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OrdinalStatic {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
    Char(i8),
    UnsignedChar(u8),
    ZeroBytes(i32),
    String(Vec<i8>, bool),
    Pointer(String),
}

impl OrdinalStatic {
    pub fn to_constant(&self) -> Constant {
        match self {
            OrdinalStatic::Integer(i) => Constant::Integer(*i),
            OrdinalStatic::Long(l) => Constant::Long(*l),
            OrdinalStatic::UnsignedInteger(i) => Constant::UnsignedInteger(*i),
            OrdinalStatic::UnsignedLong(l) => Constant::UnsignedLong(*l),
            OrdinalStatic::ZeroBytes(n) => match n {
                4 => Constant::Integer(0),
                8 => Constant::Long(0),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInitial>),
    None,
}

impl InitialValue {
    pub fn initial(initial: StaticInitial) -> InitialValue {
        InitialValue::Initial(vec![initial])
    }
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub symbol_type: Type,
    pub storage: StorageInfo,
}
