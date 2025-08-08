use crate::compiler::types::{ComparableStatic, Constant, StaticInitialiser, Type};

impl Constant {
    pub fn convert_to(&self, target: &Type) -> StaticInitialiser {
        if matches!(target, Type::Pointer(_)) {
            self.convert_to_pointer()
        } else {
            match self {
                Constant::Double(_) => Self::convert_double_to(self, target),
                _ => Self::convert_ordinal_to(self, target),
            }
        }
    }

    pub fn convert_ordinal_to(&self, target: &Type) -> StaticInitialiser {
        // match target {}
        match self {
            Constant::Integer(i) => StaticInitialiser::from_number(*i, target),
            Constant::Long(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitialiser::from_number(*i, target),
            Constant::Char(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedChar(i) => StaticInitialiser::from_number(*i, target),
            Constant::Double(_) => unreachable!(),
        }
    }
    pub fn convert_double_to(&self, target: &Type) -> StaticInitialiser {
        // match target {}
        match self {
            Constant::Integer(i) => StaticInitialiser::from_number(*i, target),
            Constant::Long(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitialiser::from_number(*i, target),
            Constant::Double(i) => StaticInitialiser::from_double(*i, target),
            Constant::Char(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedChar(i) => StaticInitialiser::from_number(*i, target),
        }
    }
    pub fn convert_to_pointer(&self) -> StaticInitialiser {
        match self {
            Constant::Integer(0)
            | Constant::Long(0)
            | Constant::UnsignedInteger(0)
            | Constant::UnsignedLong(0) => {
                StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(8))
            }
            _ => unreachable!(),
        }
    }

    // purely-for-utility function for getting constants (almost always 0 or 1) in the appropriate type
    pub fn get_typed(value: i64, target: &Type) -> Constant {
        match target {
            Type::Integer => Constant::Integer(value.try_into().unwrap()),
            Type::Long => Constant::Long(value),
            Type::UnsignedInteger => Constant::UnsignedInteger(value.try_into().unwrap()),
            Type::UnsignedLong => Constant::UnsignedLong(value.try_into().unwrap()),
            // adding a constant to a pointer is only reasonable if the second operand is Long
            Type::Pointer(_) => Constant::Long(value),
            Type::Char => Constant::Char(value.try_into().unwrap()),
            Type::SignedChar => Constant::Char(value.try_into().unwrap()),
            Type::UnsignedChar => Constant::UnsignedChar(value.try_into().unwrap()),

            Type::Function(_, _) => unreachable!(),
            Type::Array(..) => unreachable!(),
            Type::Double => panic!("Can't use get_typed_constant to generate a double"),
        }
    }

    pub fn get_double(value: f64) -> Constant {
        Constant::Double(value)
    }

    pub fn zero(target: &Type) -> StaticInitialiser {
        StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(target.get_size()))
    }
}
