use super::Type;

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
