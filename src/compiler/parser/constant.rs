use conv::{ApproxInto, ConvUtil, RoundToZero, Wrapping};

use crate::compiler::types::{Constant, OrdinalStatic, StaticInitial, Type};

impl Constant {
    pub fn convert_to(&self, target: &Type) -> StaticInitial {
        if matches!(target, Type::Pointer(_)) {
            self.convert_to_pointer()
        } else {
            match self {
                Constant::Double(_) => Self::convert_double_to(self, target),
                _ => Self::convert_ordinal_to(self, target),
            }
        }
    }

    pub fn zero(target: &Type) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(target.get_size()))
    }

    pub fn convert_ordinal_to(&self, target: &Type) -> StaticInitial {
        // match target {}
        match self {
            Constant::Integer(i) => StaticInitial::from_number(*i, target),
            Constant::Long(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitial::from_number(*i, target),
            Constant::Double(_) => unreachable!(),
        }
    }
    pub fn convert_double_to(&self, target: &Type) -> StaticInitial {
        // match target {}
        match self {
            Constant::Integer(i) => StaticInitial::from_number(*i, target),
            Constant::Long(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitial::from_number(*i, target),
            Constant::Double(i) => StaticInitial::from_double(*i, target),
        }
    }
    pub fn convert_to_pointer(&self) -> StaticInitial {
        match self {
            Constant::Integer(0)
            | Constant::Long(0)
            | Constant::UnsignedInteger(0)
            | Constant::UnsignedLong(0) => StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(8)),
            _ => unreachable!(),
        }
    }
}

trait ApproximableOrdinal:
    ApproxInto<i32, Wrapping>
    + ApproxInto<i64, Wrapping>
    + ApproxInto<u32, Wrapping>
    + ApproxInto<u64, Wrapping>
    + ApproxInto<f64>
    + Copy
{
}
impl ApproximableOrdinal for i32 {}
impl ApproximableOrdinal for i64 {}
impl ApproximableOrdinal for u32 {}
impl ApproximableOrdinal for u64 {}

impl StaticInitial {
    fn from_double(i: f64, target: &Type) -> StaticInitial {
        match target {
            Type::Integer => StaticInitial::integer_from_double(i),
            Type::Long => StaticInitial::long_from_double(i),
            Type::UnsignedInteger => StaticInitial::unsigned_integer_from_double(i),
            Type::UnsignedLong => StaticInitial::unsigned_long_from_double(i),
            Type::Double => StaticInitial::Double(i),
            Type::Function(_, _) => unreachable!(),
            Type::Pointer(_) => unreachable!(),
            Type::Array(..) => unreachable!(),
        }
    }

    fn from_number<T: ApproximableOrdinal>(i: T, target: &Type) -> StaticInitial {
        // make sure this isn't -0.0, for example
        match target {
            Type::Integer => StaticInitial::integer(i),
            Type::Long => StaticInitial::long(i),
            Type::UnsignedInteger => StaticInitial::unsigned_integer(i),
            Type::UnsignedLong => StaticInitial::unsigned_long(i),
            Type::Double => StaticInitial::double(i),
            Type::Pointer(_) | Type::Array(..) => {
                let value = StaticInitial::unsigned_long(i);
                if !matches!(
                    value,
                    StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(0))
                ) {
                    panic!("Invalid numeric value for initialising pointer");
                }
                value
            }
            Type::Function(_, _) => unreachable!(),
        }
    }

    fn integer<T: ApproxInto<i32, Wrapping>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::Integer.get_size()));
        }
        StaticInitial::Ordinal(OrdinalStatic::Integer(real_i))
    }
    fn long<T: ApproxInto<i64, Wrapping>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::Long.get_size()));
        }
        StaticInitial::Ordinal(OrdinalStatic::Long(real_i))
    }
    fn unsigned_integer<T: ApproxInto<u32, Wrapping>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(),
            ));
        }
        StaticInitial::Ordinal(OrdinalStatic::UnsignedInteger(real_i))
    }
    fn unsigned_long<T: ApproxInto<u64, Wrapping>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::UnsignedLong.get_size()));
        }
        StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(real_i))
    }

    fn double<T: ApproxInto<f64>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0. {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::Double.get_size()));
        }
        StaticInitial::Double(ConvUtil::approx_as(real_i).unwrap())
    }

    fn integer_from_double<T: ApproxInto<i32, RoundToZero>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::Integer.get_size()));
        }
        StaticInitial::Ordinal(OrdinalStatic::Integer(real_i))
    }
    fn long_from_double<T: ApproxInto<i64, RoundToZero>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::Long.get_size()));
        }
        StaticInitial::Ordinal(OrdinalStatic::Long(real_i))
    }
    fn unsigned_integer_from_double<T: ApproxInto<u32, RoundToZero>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(),
            ));
        }
        StaticInitial::Ordinal(OrdinalStatic::UnsignedInteger(real_i))
    }
    fn unsigned_long_from_double<T: ApproxInto<u64, RoundToZero>>(i: T) -> StaticInitial {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitial::Ordinal(OrdinalStatic::ZeroBytes(Type::UnsignedLong.get_size()));
        }
        StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(real_i))
    }
}
