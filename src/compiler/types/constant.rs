use std::{
    collections::HashMap,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub},
};

use crate::compiler::{
    parser::StructInfo,
    types::{ComparableStatic, Constant, StaticInitialiser, Type},
};

use super::{one_double_is_negative_zero, one_float_is_negative_zero};

impl Constant {
    pub fn static_convert_to(&self, target: &Type) -> StaticInitialiser {
        if matches!(target, Type::Pointer(_)) {
            self.static_convert_to_pointer()
        } else {
            Constant::static_convert_number_to(self, target)
            // match self {
            //     Constant::Double(_) => Self::static_convert_double_to(self, target),
            //     _ => Self::static_convert_number_to(self, target),
            // }
        }
    }

    pub fn static_convert_number_to(&self, target: &Type) -> StaticInitialiser {
        match self {
            Constant::Integer(i) => StaticInitialiser::from_number(*i, target),
            Constant::Long(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitialiser::from_number(*i, target),
            Constant::Char(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedChar(i) => StaticInitialiser::from_number(*i, target),
            Constant::Short(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedShort(i) => StaticInitialiser::from_number(*i, target),
            Constant::Float(i) => StaticInitialiser::from_double((*i).into(), target),
            Constant::Double(i) => StaticInitialiser::from_double(*i, target),
            Constant::LongLong(i) => StaticInitialiser::from_number(*i, target),
            Constant::UnsignedLongLong(i) => StaticInitialiser::from_number(*i, target),
        }
    }
    // pub fn static_convert_double_to(&self, target: &Type) -> StaticInitialiser {
    //     // match target {}
    //     match self {
    //         Constant::Integer(i) => StaticInitialiser::from_number(*i, target),
    //         Constant::Long(i) => StaticInitialiser::from_number(*i, target),
    //         Constant::UnsignedInteger(i) => StaticInitialiser::from_number(*i, target),
    //         Constant::UnsignedLong(i) => StaticInitialiser::from_number(*i, target),
    //         Constant::Double(i) => StaticInitialiser::from_double(*i, target),
    //         Constant::Char(i) => StaticInitialiser::from_number(*i, target),
    //         Constant::UnsignedChar(i) => StaticInitialiser::from_number(*i, target),
    //     }
    // }
    pub fn static_convert_to_pointer(&self) -> StaticInitialiser {
        match self {
            Constant::Integer(0)
            | Constant::Long(0)
            | Constant::LongLong(0)
            | Constant::UnsignedInteger(0)
            | Constant::UnsignedLong(0)
            | Constant::UnsignedLongLong(0) => {
                StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(8))
            }
            _ => unreachable!(),
        }
    }

    // purely-for-utility function for getting constants (almost always 0 or 1) in the appropriate type
    pub fn get_typed(value: i64, target: &Type) -> Constant {
        match target {
            Type::Integer => Constant::Integer(value.try_into().unwrap()),
            Type::Long => Constant::Long(value),
            Type::Short => Constant::Short(value.try_into().unwrap()),
            Type::LongLong => Constant::LongLong(value),
            Type::UnsignedInteger => Constant::UnsignedInteger(value.try_into().unwrap()),
            Type::UnsignedLong => Constant::UnsignedLong(value.try_into().unwrap()),
            Type::UnsignedShort => Constant::UnsignedShort(value.try_into().unwrap()),
            Type::UnsignedLongLong => Constant::UnsignedLongLong(value.try_into().unwrap()),
            // adding a constant to a pointer is only reasonable if the second operand is Long
            Type::Pointer(_) => Constant::Long(value),
            Type::Char => Constant::Char(value.try_into().unwrap()),
            Type::SignedChar => Constant::Char(value.try_into().unwrap()),
            Type::UnsignedChar => Constant::UnsignedChar(value.try_into().unwrap()),

            Type::Function(_, _) => unreachable!(),
            Type::Array(..) => unreachable!(),
            Type::Float => panic!("Can't use get_typed_constant to generate a float"),
            Type::Double => panic!("Can't use get_typed_constant to generate a double"),
            Type::Void => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
        }
    }

    pub fn get_typed_float(value: f64, target: &Type) -> Constant {
        match target {
            Type::Double => Constant::Double(value),
            Type::Float => Constant::Float(value as f32),
            _ => unreachable!(),
        }
    }

    pub fn zero(target: &Type, structs: &mut HashMap<String, StructInfo>) -> StaticInitialiser {
        StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(target.get_size(structs)))
    }

    pub fn is_zero(&self) -> bool {
        matches!(
            self,
            Constant::Integer(0)
                | Constant::Long(0)
                | Constant::UnsignedInteger(0)
                | Constant::UnsignedLong(0)
                | Constant::Double(0.0)
                | Constant::Char(0)
                | Constant::UnsignedChar(0)
        )
    }

    pub fn promote(self) -> (Constant, bool) {
        match &self {
            Constant::Char(i) => (Constant::Integer((*i).into()), true),
            Constant::UnsignedChar(i) => (Constant::UnsignedInteger((*i).into()), true),
            Constant::Short(i) => (Constant::Integer((*i).into()), true),
            Constant::UnsignedShort(i) => (Constant::UnsignedInteger((*i).into()), true),
            _ => (self, false),
        }
    }

    pub fn complement(&self) -> Constant {
        match self {
            Constant::Integer(i) => Constant::Integer(!i),
            Constant::Long(i) => Constant::Long(!i),
            Constant::LongLong(i) => Constant::LongLong(!i),
            Constant::UnsignedInteger(i) => Constant::UnsignedInteger(!i),
            Constant::UnsignedLong(i) => Constant::UnsignedLong(!i),
            Constant::UnsignedLongLong(i) => Constant::UnsignedLongLong(!i),
            Constant::Float(_i) => unreachable!(),
            Constant::Double(_i) => unreachable!(),
            Constant::Char(i) => Constant::Char(!i),
            Constant::UnsignedChar(i) => Constant::UnsignedChar(!i),
            Constant::Short(i) => Constant::Short(!i),
            Constant::UnsignedShort(i) => Constant::UnsignedShort(!i),
        }
    }

    pub fn convert_to(&self, target: &Type) -> Constant {
        match target {
            Type::Integer => self.to_int(),
            Type::Long => self.to_long(),
            Type::UnsignedInteger => self.to_uint(),
            Type::UnsignedLong => self.to_ulong(),
            Type::Float => self.to_float(),
            Type::Double => self.to_double(),
            Type::Function(_, _) => unreachable!(),
            Type::Array(_, _) => unreachable!(),
            Type::Pointer(_) => self.to_long(),
            Type::Char => self.to_char(),
            Type::SignedChar => self.to_char(),
            Type::UnsignedChar => self.to_uchar(),
            Type::Void => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
            Type::Short => self.to_short(),
            Type::UnsignedShort => self.to_ushort(),
            Type::LongLong => self.to_long_long(),
            Type::UnsignedLongLong => self.to_ulong_long(),
        }
    }

    pub fn to_int(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i,
            Constant::Long(i) => *i as i32,
            Constant::UnsignedInteger(i) => *i as i32,
            Constant::UnsignedLong(i) => *i as i32,
            Constant::Float(i) => *i as i32,
            Constant::Double(i) => *i as i32,
            Constant::Char(i) => *i as i32,
            Constant::UnsignedChar(i) => *i as i32,
            Constant::Short(i) => *i as i32,
            Constant::UnsignedShort(i) => *i as i32,
            Constant::LongLong(i) => *i as i32,
            Constant::UnsignedLongLong(i) => *i as i32,
        };
        Constant::Integer(val)
    }
    pub fn to_uint(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u32,
            Constant::Long(i) => *i as u32,
            Constant::UnsignedInteger(i) => *i,
            Constant::UnsignedLong(i) => *i as u32,
            Constant::Float(i) => *i as u32,
            Constant::Double(i) => *i as u32,
            Constant::Char(i) => *i as u32,
            Constant::UnsignedChar(i) => *i as u32,
            Constant::Short(i) => *i as u32,
            Constant::UnsignedShort(i) => *i as u32,
            Constant::LongLong(i) => *i as u32,
            Constant::UnsignedLongLong(i) => *i as u32,
        };
        Constant::UnsignedInteger(val)
    }
    pub fn to_long(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as i64,
            Constant::Long(i) => *i,
            Constant::UnsignedInteger(i) => *i as i64,
            Constant::UnsignedLong(i) => *i as i64,
            Constant::Float(i) => *i as i64,
            Constant::Double(i) => *i as i64,
            Constant::Char(i) => *i as i64,
            Constant::UnsignedChar(i) => *i as i64,
            Constant::Short(i) => *i as i64,
            Constant::UnsignedShort(i) => *i as i64,
            Constant::LongLong(i) => *i,
            Constant::UnsignedLongLong(i) => *i as i64,
        };
        Constant::Long(val)
    }
    pub fn to_ulong(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u64,
            Constant::Long(i) => *i as u64,
            Constant::UnsignedInteger(i) => *i as u64,
            Constant::UnsignedLong(i) => *i,
            Constant::Float(i) => *i as u64,
            Constant::Double(i) => *i as u64,
            Constant::Char(i) => *i as u64,
            Constant::UnsignedChar(i) => *i as u64,
            Constant::Short(i) => *i as u64,
            Constant::UnsignedShort(i) => *i as u64,
            Constant::LongLong(i) => *i as u64,
            Constant::UnsignedLongLong(i) => *i,
        };
        Constant::UnsignedLong(val)
    }

    pub fn to_float(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as f32,
            Constant::Long(i) => *i as f32,
            Constant::UnsignedInteger(i) => *i as f32,
            Constant::UnsignedLong(i) => *i as f32,
            Constant::Float(i) => *i,
            Constant::Double(i) => *i as f32,
            Constant::Char(i) => *i as f32,
            Constant::UnsignedChar(i) => *i as f32,
            Constant::Short(i) => *i as f32,
            Constant::UnsignedShort(i) => *i as f32,
            Constant::LongLong(i) => *i as f32,
            Constant::UnsignedLongLong(i) => *i as f32,
        };
        Constant::Float(val)
    }
    pub fn to_double(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as f64,
            Constant::Long(i) => *i as f64,
            Constant::UnsignedInteger(i) => *i as f64,
            Constant::UnsignedLong(i) => *i as f64,
            Constant::Float(i) => *i as f64,
            Constant::Double(i) => *i,
            Constant::Char(i) => *i as f64,
            Constant::UnsignedChar(i) => *i as f64,
            Constant::Short(i) => *i as f64,
            Constant::UnsignedShort(i) => *i as f64,
            Constant::LongLong(i) => *i as f64,
            Constant::UnsignedLongLong(i) => *i as f64,
        };
        Constant::Double(val)
    }
    pub fn to_char(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as i8,
            Constant::Long(i) => *i as i8,
            Constant::UnsignedInteger(i) => *i as i8,
            Constant::UnsignedLong(i) => *i as i8,
            Constant::Float(i) => *i as i8,
            Constant::Double(i) => *i as i8,
            Constant::Char(i) => *i,
            Constant::UnsignedChar(i) => *i as i8,
            Constant::Short(i) => *i as i8,
            Constant::UnsignedShort(i) => *i as i8,
            Constant::LongLong(i) => *i as i8,
            Constant::UnsignedLongLong(i) => *i as i8,
        };
        Constant::Char(val)
    }
    pub fn to_uchar(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u8,
            Constant::Long(i) => *i as u8,
            Constant::UnsignedInteger(i) => *i as u8,
            Constant::UnsignedLong(i) => *i as u8,
            Constant::Float(i) => *i as u8,
            Constant::Double(i) => *i as u8,
            Constant::Char(i) => *i as u8,
            Constant::UnsignedChar(i) => *i,
            Constant::Short(i) => *i as u8,
            Constant::UnsignedShort(i) => *i as u8,
            Constant::LongLong(i) => *i as u8,
            Constant::UnsignedLongLong(i) => *i as u8,
        };
        Constant::UnsignedChar(val)
    }
    pub fn to_short(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as i16,
            Constant::Long(i) => *i as i16,
            Constant::UnsignedInteger(i) => *i as i16,
            Constant::UnsignedLong(i) => *i as i16,
            Constant::Float(i) => *i as i16,
            Constant::Double(i) => *i as i16,
            Constant::Char(i) => *i as i16,
            Constant::UnsignedChar(i) => *i as i16,
            Constant::Short(i) => *i,
            Constant::UnsignedShort(i) => *i as i16,
            Constant::LongLong(i) => *i as i16,
            Constant::UnsignedLongLong(i) => *i as i16,
        };
        Constant::Short(val)
    }
    pub fn to_ushort(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u16,
            Constant::Long(i) => *i as u16,
            Constant::UnsignedInteger(i) => *i as u16,
            Constant::UnsignedLong(i) => *i as u16,
            Constant::Float(i) => *i as u16,
            Constant::Double(i) => *i as u16,
            Constant::Char(i) => *i as u16,
            Constant::UnsignedChar(i) => *i as u16,
            Constant::Short(i) => *i as u16,
            Constant::UnsignedShort(i) => *i,
            Constant::LongLong(i) => *i as u16,
            Constant::UnsignedLongLong(i) => *i as u16,
        };
        Constant::UnsignedShort(val)
    }
    pub fn to_long_long(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as i64,
            Constant::Long(i) => *i,
            Constant::UnsignedInteger(i) => *i as i64,
            Constant::UnsignedLong(i) => *i as i64,
            Constant::Float(i) => *i as i64,
            Constant::Double(i) => *i as i64,
            Constant::Char(i) => *i as i64,
            Constant::UnsignedChar(i) => *i as i64,
            Constant::Short(i) => *i as i64,
            Constant::UnsignedShort(i) => *i as i64,
            Constant::LongLong(i) => *i,
            Constant::UnsignedLongLong(i) => *i as i64,
        };
        Constant::LongLong(val)
    }
    pub fn to_ulong_long(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u64,
            Constant::Long(i) => *i as u64,
            Constant::UnsignedInteger(i) => *i as u64,
            Constant::UnsignedLong(i) => *i,
            Constant::Float(i) => *i as u64,
            Constant::Double(i) => *i as u64,
            Constant::Char(i) => *i as u64,
            Constant::UnsignedChar(i) => *i as u64,
            Constant::Short(i) => *i as u64,
            Constant::UnsignedShort(i) => *i as u64,
            Constant::LongLong(i) => *i as u64,
            Constant::UnsignedLongLong(i) => *i,
        };
        Constant::UnsignedLongLong(val)
    }

    // used to un-override the equality behaviour of nan operands during constant folding
    pub fn double_eq(&self, other: &Constant) -> bool {
        match (self, other) {
            (Constant::Double(left), Constant::Double(right)) => left == right,
            _ => self.eq(other),
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        // WARNING: adding any more types means you have to populate this field, or the constant
        // folding pass loops indefinitely.
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Long(a), Self::Long(b)) => a == b,
            (Self::UnsignedInteger(a), Self::UnsignedInteger(b)) => a == b,
            (Self::UnsignedLong(a), Self::UnsignedLong(b)) => a == b,
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
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::UnsignedChar(a), Self::UnsignedChar(b)) => a == b,
            (Self::Short(a), Self::Short(b)) => a == b,
            (Self::UnsignedShort(a), Self::UnsignedShort(b)) => a == b,
            (Self::LongLong(a), Self::LongLong(b)) => a == b,
            (Self::UnsignedLongLong(a), Self::UnsignedLongLong(b)) => a == b,
            _ => false,
        }
    }
}

impl Add for Constant {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a + b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a + b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a + b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a + b),
            (Constant::Double(a), Constant::Double(b)) => Constant::Double(a + b),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a + b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a + b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a + b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a + b)
            }
            (Constant::Float(a), Constant::Float(b)) => Constant::Float(a + b),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a + b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a + b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl Sub for Constant {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a - b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a - b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a - b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a - b),
            (Constant::Double(a), Constant::Double(b)) => Constant::Double(a - b),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a - b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a - b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a - b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a - b)
            }
            (Constant::Float(a), Constant::Float(b)) => Constant::Float(a - b),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a - b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a - b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl Mul for Constant {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a * b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a * b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a * b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a * b),
            (Constant::Double(a), Constant::Double(b)) => Constant::Double(a * b),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a * b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a * b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a * b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a * b)
            }
            (Constant::Float(a), Constant::Float(b)) => Constant::Float(a * b),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a * b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a * b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl Div for Constant {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() && !matches!(self, Constant::Double(_)) {
            // dividing by zero is undefined behaviour. Ignore the division so Rust doesn't crash
            return self;
        }
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a / b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a / b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a / b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a / b),
            (Constant::Double(a), Constant::Double(b)) => {
                // double check the value is not -0.0 (0.0 matches -0.0 in Rust)
                // if a == 0.0 {
                //     Constant::Double(a)
                // } else {
                Constant::Double(a / b)
                // }
            }
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a / b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a / b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a / b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a / b)
            }
            (Constant::Float(a), Constant::Float(b)) => Constant::Float(a / b),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a / b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a / b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl Rem for Constant {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            // mod with zero is undefined behaviour. Ignore the division so Rust doesn't crash
            return self;
        }
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a % b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a % b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a % b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a % b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a % b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a % b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a % b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a % b)
            }
            (Constant::Float(a), Constant::Float(b)) => Constant::Float(a % b),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a % b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a % b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Constant::Integer(a), Constant::Integer(b)) => Some(a.cmp(b)),
            (Constant::Long(a), Constant::Long(b)) => Some(a.cmp(b)),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => Some(a.cmp(b)),
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Some(a.cmp(b)),
            // unordered results return None here, neat!
            (Constant::Double(a), Constant::Double(b)) => a.partial_cmp(b),
            (Constant::Char(a), Constant::Char(b)) => Some(a.cmp(b)),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Some(a.cmp(b)),
            (Constant::Short(a), Constant::Short(b)) => Some(a.cmp(b)),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => Some(a.cmp(b)),
            (Constant::Float(a), Constant::Float(b)) => a.partial_cmp(b),
            (Constant::LongLong(a), Constant::LongLong(b)) => Some(a.cmp(b)),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => Some(a.cmp(b)),
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl BitAnd for Constant {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a & b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a & b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a & b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a & b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a & b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a & b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a & b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a & b)
            }
            (Constant::Float(_a), Constant::Float(_b)) => unreachable!(),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a & b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a & b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl BitOr for Constant {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a | b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a | b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a | b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a | b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a | b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a | b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a | b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a | b)
            }
            (Constant::Float(_a), Constant::Float(_b)) => unreachable!(),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a | b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a | b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl BitXor for Constant {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a ^ b),
            (Constant::Long(a), Constant::Long(b)) => Constant::Long(a ^ b),
            (Constant::UnsignedInteger(a), Constant::UnsignedInteger(b)) => {
                Constant::UnsignedInteger(a ^ b)
            }
            (Constant::UnsignedLong(a), Constant::UnsignedLong(b)) => Constant::UnsignedLong(a ^ b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Char(b)) => Constant::Char(a ^ b),
            (Constant::UnsignedChar(a), Constant::UnsignedChar(b)) => Constant::UnsignedChar(a ^ b),
            (Constant::Short(a), Constant::Short(b)) => Constant::Short(a ^ b),
            (Constant::UnsignedShort(a), Constant::UnsignedShort(b)) => {
                Constant::UnsignedShort(a ^ b)
            }
            (Constant::Float(_a), Constant::Float(_b)) => unreachable!(),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a ^ b),
            (Constant::UnsignedLongLong(a), Constant::UnsignedLongLong(b)) => {
                Constant::UnsignedLongLong(a ^ b)
            }
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl Shl for Constant {
    type Output = Self;

    fn shl(mut self, rhs: Self) -> Self::Output {
        let is_promoted;
        (self, is_promoted) = self.promote();
        let rhs = rhs.to_int();
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => {
                if is_promoted {
                    Constant::Char((a << b) as i8)
                } else {
                    Constant::Integer(a << b)
                }
            }
            (Constant::Long(a), Constant::Integer(b)) => Constant::Long(a << b),
            (Constant::UnsignedInteger(a), Constant::Integer(b)) => {
                if is_promoted {
                    Constant::UnsignedChar((a << b) as u8)
                } else {
                    Constant::UnsignedInteger(a << b)
                }
            }
            (Constant::UnsignedLong(a), Constant::Integer(b)) => Constant::UnsignedLong(a << b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Integer(b)) => Constant::Char(a << b),
            (Constant::UnsignedChar(a), Constant::Integer(b)) => Constant::UnsignedChar(a << b),
            (Constant::Short(a), Constant::Integer(b)) => Constant::Short(a << b),
            (Constant::UnsignedShort(a), Constant::Integer(b)) => Constant::UnsignedShort(a << b),
            (Constant::Float(_a), Constant::Integer(_b)) => unreachable!(),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a << b),
            (Constant::UnsignedLongLong(a), Constant::Integer(b)) => {
                Constant::UnsignedLongLong(a << b)
            }
            _ => unreachable!(), // can only do shl with integers as the second arg
        }
    }
}

impl Shr for Constant {
    type Output = Self;

    fn shr(mut self, rhs: Self) -> Self::Output {
        let is_promoted;
        (self, is_promoted) = self.promote();
        let rhs = rhs.to_int();
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => {
                if is_promoted {
                    Constant::Char((a >> b) as i8)
                } else {
                    Constant::Integer(a >> b)
                }
            }
            (Constant::Long(a), Constant::Integer(b)) => Constant::Long(a >> b),
            (Constant::UnsignedInteger(a), Constant::Integer(b)) => {
                if is_promoted {
                    Constant::UnsignedChar((a >> b) as u8)
                } else {
                    Constant::UnsignedInteger(a >> b)
                }
            }
            (Constant::UnsignedLong(a), Constant::Integer(b)) => Constant::UnsignedLong(a >> b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Integer(b)) => Constant::Char(a >> b),
            (Constant::UnsignedChar(a), Constant::Integer(b)) => Constant::UnsignedChar(a >> b),
            (Constant::Short(a), Constant::Integer(b)) => Constant::Short(a >> b),
            (Constant::UnsignedShort(a), Constant::Integer(b)) => Constant::UnsignedShort(a >> b),
            (Constant::Float(_a), Constant::Integer(_b)) => unreachable!(),
            (Constant::LongLong(a), Constant::LongLong(b)) => Constant::LongLong(a >> b),
            (Constant::UnsignedLongLong(a), Constant::Integer(b)) => {
                Constant::UnsignedLongLong(a >> b)
            }
            _ => unreachable!(), // can only do shr with integers as the second arg
        }
    }
}

impl Not for Constant {
    type Output = Self;

    fn not(self) -> Self::Output {
        Constant::Integer(self.is_zero() as i32)
    }
}

impl Neg for Constant {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Constant::Integer(i) => Constant::Integer(-i),
            Constant::Long(i) => Constant::Long(-i),
            Constant::UnsignedInteger(i) => Constant::UnsignedInteger(!i + 1), // undefined behaviour
            Constant::UnsignedLong(i) => Constant::UnsignedLong(!i + 1), // undefined behaviour
            Constant::Float(i) => Constant::Float(-i),
            Constant::Double(i) => Constant::Double(-i),
            Constant::Char(i) => Constant::Char(!i),
            Constant::UnsignedChar(i) => Constant::UnsignedChar(!i + 1), // undefined behaviour
            Constant::Short(i) => Constant::Short(!i),
            Constant::UnsignedShort(i) => Constant::UnsignedShort(!i + 1), // undefined behaviour
            Constant::LongLong(i) => Constant::LongLong(!i),
            Constant::UnsignedLongLong(i) => Constant::UnsignedLongLong(!i + 1), // undefined behaviour
        }
    }
}
