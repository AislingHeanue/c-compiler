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
            Type::Array(..) => 8,  // arrays are like pointers except that they aren't
            Type::Function(_, _) => unreachable!(),
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
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Double => false,
            _ => self.is_arithmetic(),
        }
    }

    // pub fn is_scalar(&self) -> bool {
    //     match self {
    //         Type::Array(..) => false,
    //         _ => true,
    //     }
    // }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
    Double(f64),
}

#[derive(Debug, Clone)]
pub enum StorageInfo {
    // is_defined and global (ie non-static)
    Function(bool, bool),
    // initializer and global
    Static(InitialValue, bool),
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
}

impl StaticInitial {
    pub fn is_zero(&self) -> bool {
        matches!(
            self,
            // doubles are never treated as zero initialisations to sidestep confusion over 0.0
            // and -0.0
            StaticInitial::Ordinal(OrdinalStatic::Integer(0))
                | StaticInitial::Ordinal(OrdinalStatic::Long(0))
                | StaticInitial::Ordinal(OrdinalStatic::UnsignedInteger(0))
                | StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(0))
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OrdinalStatic {
    Integer(i32),
    Long(i64),
    UnsignedInteger(u32),
    UnsignedLong(u64),
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
