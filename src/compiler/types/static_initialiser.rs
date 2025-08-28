use std::collections::HashMap;

use conv::{ApproxInto, ConvUtil, RoundToZero, Wrapping};

use super::{ComparableStatic, Constant, InitialValue, StaticInitialiser, Type};

pub trait ApproximableOrdinal:
    ApproxInto<i8, Wrapping>
    + ApproxInto<i16, Wrapping>
    + ApproxInto<i32, Wrapping>
    + ApproxInto<i64, Wrapping>
    + ApproxInto<u8, Wrapping>
    + ApproxInto<u16, Wrapping>
    + ApproxInto<u32, Wrapping>
    + ApproxInto<u64, Wrapping>
    + ApproxInto<f32>
    + ApproxInto<f64>
    + Copy
{
}
impl ApproximableOrdinal for i8 {}
impl ApproximableOrdinal for i16 {}
impl ApproximableOrdinal for i32 {}
impl ApproximableOrdinal for i64 {}
impl ApproximableOrdinal for u8 {}
impl ApproximableOrdinal for u16 {}
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
            Type::LongLong => StaticInitialiser::long_from_double(i),
            Type::Short => StaticInitialiser::short_from_double(i),
            Type::UnsignedInteger => StaticInitialiser::unsigned_integer_from_double(i),
            Type::UnsignedLong => StaticInitialiser::unsigned_long_from_double(i),
            Type::UnsignedLongLong => StaticInitialiser::unsigned_long_from_double(i),
            Type::UnsignedShort => StaticInitialiser::unsigned_short_from_double(i),
            Type::Float => StaticInitialiser::float_from_double(i),
            Type::Double => StaticInitialiser::Double(i),
            Type::LongDouble => StaticInitialiser::LongDouble(i),
            Type::Char => StaticInitialiser::char_from_double(i),
            Type::SignedChar => StaticInitialiser::char_from_double(i),
            Type::UnsignedChar => StaticInitialiser::unsigned_char_from_double(i),
            Type::Function(_, _) => unreachable!(),
            Type::Pointer(_, _) => unreachable!(),
            Type::Array(..) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
        }
    }

    pub fn from_number<T: ApproximableOrdinal>(i: T, target: &Type) -> StaticInitialiser {
        match target {
            Type::Integer => StaticInitialiser::integer(i),
            Type::Long => StaticInitialiser::long(i),
            Type::Short => StaticInitialiser::short(i),
            Type::LongLong => StaticInitialiser::long_long(i),
            Type::UnsignedInteger => StaticInitialiser::unsigned_integer(i),
            Type::UnsignedLong => StaticInitialiser::unsigned_long(i),
            Type::UnsignedShort => StaticInitialiser::unsigned_short(i),
            Type::UnsignedLongLong => StaticInitialiser::unsigned_long_long(i),
            Type::Float => StaticInitialiser::float(i),
            Type::Double => StaticInitialiser::double(i),
            Type::LongDouble => StaticInitialiser::long_double(i),
            Type::Char => StaticInitialiser::char(i),
            Type::SignedChar => StaticInitialiser::char(i),
            Type::UnsignedChar => StaticInitialiser::unsigned_char(i),
            Type::Pointer(_, _) | Type::Array(..) => {
                let value = StaticInitialiser::unsigned_long(i);
                if !matches!(
                    value,
                    StaticInitialiser::Comparable(ComparableStatic::UnsignedLong(0))
                ) {
                    panic!("Invalid numeric value for initialising pointer");
                }
                value
            }
            Type::Function(_, _) => unreachable!(),
            Type::Void => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
        }
    }

    fn char<T: ApproxInto<i8, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Char.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Char(real_i))
    }

    fn short<T: ApproxInto<i16, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Short.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Short(real_i))
    }

    fn integer<T: ApproxInto<i32, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Integer.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Integer(real_i))
    }

    fn long<T: ApproxInto<i64, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Long.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Long(real_i))
    }

    fn long_long<T: ApproxInto<i64, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::LongLong.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::LongLong(real_i))
    }

    fn unsigned_char<T: ApproxInto<u8, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedChar.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedChar(real_i))
    }

    fn unsigned_short<T: ApproxInto<u16, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedShort.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedShort(real_i))
    }

    fn unsigned_integer<T: ApproxInto<u32, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedInteger(real_i))
    }

    fn unsigned_long<T: ApproxInto<u64, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedLong.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedLong(real_i))
    }

    fn unsigned_long_long<T: ApproxInto<u64, Wrapping>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedLongLong.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedLongLong(real_i))
    }

    fn float<T: ApproxInto<f32>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as().unwrap();
        if real_i == 0. {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Float.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Float(ConvUtil::approx_as(real_i).unwrap())
    }

    fn double<T: ApproxInto<f64>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0. {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Double.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Double(ConvUtil::approx_as(real_i).unwrap())
    }

    fn long_double<T: ApproxInto<f64>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0. {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::LongDouble.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::LongDouble(ConvUtil::approx_as(real_i).unwrap())
    }

    fn char_from_double<T: ApproxInto<i8, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Char.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Char(real_i))
    }

    fn short_from_double<T: ApproxInto<i16, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Short.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Short(real_i))
    }

    fn integer_from_double<T: ApproxInto<i32, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Integer.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Integer(real_i))
    }

    fn long_from_double<T: ApproxInto<i64, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Long.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::Long(real_i))
    }

    fn unsigned_char_from_double<T: ApproxInto<u8, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedChar(real_i))
    }

    fn unsigned_short_from_double<T: ApproxInto<u16, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedShort.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedShort(real_i))
    }

    fn unsigned_integer_from_double<T: ApproxInto<u32, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedInteger.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedInteger(real_i))
    }

    fn unsigned_long_from_double<T: ApproxInto<u64, RoundToZero>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as_by().unwrap();
        if real_i == 0 {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::UnsignedLong.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Comparable(ComparableStatic::UnsignedLong(real_i))
    }

    fn float_from_double<T: ApproxInto<f32>>(i: T) -> StaticInitialiser {
        let real_i = i.approx_as().unwrap();
        if real_i == 0. {
            return StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                Type::Float.get_size(&mut HashMap::new()),
            ));
        }
        StaticInitialiser::Float(ConvUtil::approx_as(real_i).unwrap())
    }
}

impl ComparableStatic {
    // NOTE: this function only needs to be updated for types that aren't promoted to in
    pub fn to_constant(&self) -> Constant {
        match self {
            ComparableStatic::Integer(i) => Constant::Integer(*i),
            ComparableStatic::Long(l) => Constant::Long(*l),
            ComparableStatic::LongLong(l) => Constant::LongLong(*l),
            ComparableStatic::UnsignedInteger(i) => Constant::UnsignedInteger(*i),
            ComparableStatic::UnsignedLong(l) => Constant::UnsignedLong(*l),
            ComparableStatic::UnsignedLongLong(l) => Constant::UnsignedLongLong(*l),
            ComparableStatic::ZeroBytes(n) => match n {
                4 => Constant::Integer(0),
                8 => Constant::Long(0),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
