use std::collections::HashMap;

use conv::{ApproxInto, ConvUtil, RoundToZero, Wrapping};

use super::{ComparableStatic, Constant, InitialValue, StaticInitialiser, Type};

pub trait ApproximableOrdinal:
    ApproxInto<i8, Wrapping>
    + ApproxInto<i32, Wrapping>
    + ApproxInto<i64, Wrapping>
    + ApproxInto<u8, Wrapping>
    + ApproxInto<u32, Wrapping>
    + ApproxInto<u64, Wrapping>
    + ApproxInto<f64>
    + Copy
{
}
impl ApproximableOrdinal for i8 {}
impl ApproximableOrdinal for i32 {}
impl ApproximableOrdinal for i64 {}
impl ApproximableOrdinal for u8 {}
impl ApproximableOrdinal for u32 {}
impl ApproximableOrdinal for u64 {}

impl InitialValue {
    pub fn initial(initial: StaticInitialiser) -> InitialValue {
        InitialValue::Initial(vec![initial])
    }
}

impl StaticInitialiser {
    pub fn from_double(i: f64, target: &Type) -> StaticInitialiser {
        match target {
            Type::Integer => StaticInitialiser::integer_from_double(i),
            Type::Long => StaticInitialiser::long_from_double(i),
            Type::UnsignedInteger => StaticInitialiser::unsigned_integer_from_double(i),
            Type::UnsignedLong => StaticInitialiser::unsigned_long_from_double(i),
            Type::Double => StaticInitialiser::Double(i),
            Type::Char => StaticInitialiser::char_from_double(i),
            Type::SignedChar => StaticInitialiser::char_from_double(i),
            Type::UnsignedChar => StaticInitialiser::unsigned_char_from_double(i),
            Type::Function(_, _) => unreachable!(),
            Type::Pointer(_) => unreachable!(),
            Type::Array(..) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Struct(_) => unreachable!(),
        }
    }

    pub fn from_number<T: ApproximableOrdinal>(i: T, target: &Type) -> StaticInitialiser {
        // make sure this isn't -0.0, for example
        match target {
            Type::Integer => StaticInitialiser::integer(i),
            Type::Long => StaticInitialiser::long(i),
            Type::UnsignedInteger => StaticInitialiser::unsigned_integer(i),
            Type::UnsignedLong => StaticInitialiser::unsigned_long(i),
            Type::Double => StaticInitialiser::double(i),
            Type::Char => StaticInitialiser::char(i),
            Type::SignedChar => StaticInitialiser::char(i),
            Type::UnsignedChar => StaticInitialiser::unsigned_char(i),
            Type::Pointer(_) | Type::Array(..) => {
                let value = StaticInitialiser::unsigned_long(i);
                if !matches!(
                    value,
                    StaticInitialiser::Ordinal(ComparableStatic::UnsignedLong(0))
                ) {
                    panic!("Invalid numeric value for initialising pointer");
                }
                value
            }
            Type::Function(_, _) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Struct(_) => unreachable!(),
        }
    }

    fn char<T: ApproxInto<i8, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Char.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::Char(real_i))
    }

    fn integer<T: ApproxInto<i32, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Integer.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::Integer(real_i))
    }

    fn long<T: ApproxInto<i64, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Long.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::Long(real_i))
    }

    fn unsigned_char<T: ApproxInto<u8, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::UnsignedChar.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::UnsignedChar(real_i))
    }

    fn unsigned_integer<T: ApproxInto<u32, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::UnsignedInteger(real_i))
    }

    fn unsigned_long<T: ApproxInto<u64, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::UnsignedLong.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::UnsignedLong(real_i))
    }

    fn double<T: ApproxInto<f64>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0. {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Double.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Double(ConvUtil::approx_as(real_i).unwrap())
    }

    fn char_from_double<T: ApproxInto<i8, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Char.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::Char(real_i))
    }
    fn integer_from_double<T: ApproxInto<i32, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Integer.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::Integer(real_i))
    }

    fn long_from_double<T: ApproxInto<i64, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::Long.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::Long(real_i))
    }

    fn unsigned_char_from_double<T: ApproxInto<u8, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::UnsignedChar(real_i))
    }

    fn unsigned_integer_from_double<T: ApproxInto<u32, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::UnsignedInteger(real_i))
    }

    fn unsigned_long_from_double<T: ApproxInto<u64, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                Type::UnsignedLong.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Ordinal(ComparableStatic::UnsignedLong(real_i))
    }
}

impl ComparableStatic {
    pub fn to_constant(&self) -> Constant {
        match self {
            ComparableStatic::Integer(i) => Constant::Integer(*i),
            ComparableStatic::Long(l) => Constant::Long(*l),
            ComparableStatic::UnsignedInteger(i) => Constant::UnsignedInteger(*i),
            ComparableStatic::UnsignedLong(l) => Constant::UnsignedLong(*l),
            ComparableStatic::ZeroBytes(n) => match n {
                4 => Constant::Integer(0),
                8 => Constant::Long(0),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
