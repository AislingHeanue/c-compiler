use std::{
    collections::HashMap,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub},
};

use crate::compiler::{
    parser::StructInfo,
    types::{ComparableStatic, Constant, StaticInitialiser, Type},
};

use super::one_double_is_negative_zero;

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
            Constant::Double(i) => StaticInitialiser::from_double(*i, target),
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
            | Constant::UnsignedInteger(0)
            | Constant::UnsignedLong(0) => {
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
            Type::Void => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
        }
    }

    pub fn get_double(value: f64) -> Constant {
        Constant::Double(value)
    }

    pub fn zero(target: &Type, structs: &mut HashMap<String, StructInfo>) -> StaticInitialiser {
        StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(target.get_size(structs)))
    }

    pub fn is_zero(&self) -> bool {
        if let Constant::Double(d) = self {
            if d.to_bits() == (-0.0_f64).to_bits() {
                // don't zero out negative zero
                return false;
            }
        }
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

    pub fn complement(&self) -> Constant {
        match self {
            Constant::Integer(i) => Constant::Integer(!i),
            Constant::Long(i) => Constant::Long(!i),
            Constant::UnsignedInteger(i) => Constant::UnsignedInteger(!i),
            Constant::UnsignedLong(i) => Constant::UnsignedLong(!i),
            Constant::Double(_i) => unreachable!(),
            Constant::Char(i) => Constant::Char(!i),
            Constant::UnsignedChar(i) => Constant::UnsignedChar(!i),
        }
    }

    pub fn convert_to(&self, target: &Type) -> Constant {
        match target {
            Type::Integer => self.to_int(),
            Type::Long => self.to_long(),
            Type::UnsignedInteger => self.to_uint(),
            Type::UnsignedLong => self.to_ulong(),
            Type::Double => self.to_double(),
            Type::Function(_, _) => unreachable!(),
            Type::Array(_, _) => unreachable!(),
            Type::Pointer(_) => self.to_long(),
            Type::Char => self.to_char(),
            Type::SignedChar => self.to_char(),
            Type::UnsignedChar => self.to_uchar(),
            Type::Void => unreachable!(),
            Type::Struct(_, _) => unreachable!(),
        }
    }

    pub fn to_int(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i,
            Constant::Long(i) => *i as i32,
            Constant::UnsignedInteger(i) => *i as i32,
            Constant::UnsignedLong(i) => *i as i32,
            Constant::Double(i) => *i as i32,
            Constant::Char(i) => *i as i32,
            Constant::UnsignedChar(i) => *i as i32,
        };
        Constant::Integer(val)
    }
    pub fn to_uint(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u32,
            Constant::Long(i) => *i as u32,
            Constant::UnsignedInteger(i) => *i,
            Constant::UnsignedLong(i) => *i as u32,
            Constant::Double(i) => *i as u32,
            Constant::Char(i) => *i as u32,
            Constant::UnsignedChar(i) => *i as u32,
        };
        Constant::UnsignedInteger(val)
    }
    pub fn to_long(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as i64,
            Constant::Long(i) => *i,
            Constant::UnsignedInteger(i) => *i as i64,
            Constant::UnsignedLong(i) => *i as i64,
            Constant::Double(i) => *i as i64,
            Constant::Char(i) => *i as i64,
            Constant::UnsignedChar(i) => *i as i64,
        };
        Constant::Long(val)
    }
    pub fn to_ulong(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u64,
            Constant::Long(i) => *i as u64,
            Constant::UnsignedInteger(i) => *i as u64,
            Constant::UnsignedLong(i) => *i,
            Constant::Double(i) => *i as u64,
            Constant::Char(i) => *i as u64,
            Constant::UnsignedChar(i) => *i as u64,
        };
        Constant::UnsignedLong(val)
    }
    pub fn to_double(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as f64,
            Constant::Long(i) => *i as f64,
            Constant::UnsignedInteger(i) => *i as f64,
            Constant::UnsignedLong(i) => *i as f64,
            Constant::Double(i) => *i,
            Constant::Char(i) => *i as f64,
            Constant::UnsignedChar(i) => *i as f64,
        };
        Constant::Double(val)
    }
    pub fn to_char(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as i8,
            Constant::Long(i) => *i as i8,
            Constant::UnsignedInteger(i) => *i as i8,
            Constant::UnsignedLong(i) => *i as i8,
            Constant::Double(i) => *i as i8,
            Constant::Char(i) => *i,
            Constant::UnsignedChar(i) => *i as i8,
        };
        Constant::Char(val)
    }
    pub fn to_uchar(&self) -> Constant {
        let val = match self {
            Constant::Integer(i) => *i as u8,
            Constant::Long(i) => *i as u8,
            Constant::UnsignedInteger(i) => *i as u8,
            Constant::UnsignedLong(i) => *i as u8,
            Constant::Double(i) => *i as u8,
            Constant::Char(i) => *i as u8,
            Constant::UnsignedChar(i) => *i,
        };
        Constant::UnsignedChar(val)
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
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
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::UnsignedChar(a), Self::UnsignedChar(b)) => a == b,
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
            _ => unreachable!(), // can only operate on 2 constants of the same type
        }
    }
}

impl Shl for Constant {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let rhs = rhs.to_int();
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a << b),
            (Constant::Long(a), Constant::Integer(b)) => Constant::Long(a << b),
            (Constant::UnsignedInteger(a), Constant::Integer(b)) => {
                Constant::UnsignedInteger(a << b)
            }
            (Constant::UnsignedLong(a), Constant::Integer(b)) => Constant::UnsignedLong(a << b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Integer(b)) => Constant::Char(a << b),
            (Constant::UnsignedChar(a), Constant::Integer(b)) => Constant::UnsignedChar(a << b),
            _ => unreachable!(), // can only do shl with integers as the second arg
        }
    }
}

impl Shr for Constant {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        let rhs = rhs.to_int();
        match (self, rhs) {
            (Constant::Integer(a), Constant::Integer(b)) => Constant::Integer(a >> b),
            (Constant::Long(a), Constant::Integer(b)) => Constant::Long(a >> b),
            (Constant::UnsignedInteger(a), Constant::Integer(b)) => {
                Constant::UnsignedInteger(a >> b)
            }
            (Constant::UnsignedLong(a), Constant::Integer(b)) => Constant::UnsignedLong(a >> b),
            (Constant::Double(_a), Constant::Double(_b)) => unreachable!(),
            (Constant::Char(a), Constant::Integer(b)) => Constant::Char(a >> b),
            (Constant::UnsignedChar(a), Constant::Integer(b)) => Constant::UnsignedChar(a >> b),
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
            Constant::Double(i) => Constant::Double(-i),
            Constant::Char(i) => Constant::Char(!i),
            Constant::UnsignedChar(i) => Constant::UnsignedChar(!i + 1), // undefined behaviour
        }
    }
}
