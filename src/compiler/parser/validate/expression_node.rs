use std::error::Error;

use crate::compiler::{
    parser::{BinaryOperatorNode, ExpressionNode, ExpressionWithoutType, UnaryOperatorNode},
    types::{Constant, Type},
};

use super::{Validate, ValidateContext, ValidationPass};

impl Validate for ExpressionNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::CheckLvalues) {
            self.validate_lvalues(context)?;
        }
        if matches!(context.pass, ValidationPass::TypeChecking) {
            self.check_types(context)?;
        }

        match self.0 {
            ExpressionWithoutType::Constant(_) => {}
            ExpressionWithoutType::Unary(_, ref mut src) => {
                src.validate(context)?;
            }
            ExpressionWithoutType::Compound(_, ref mut left, ref mut right) => {
                left.validate(context)?;
                right.validate(context)?;
            }
            ExpressionWithoutType::Binary(_, ref mut left, ref mut right) => {
                left.validate(context)?;
                right.validate(context)?;
            }
            ExpressionWithoutType::Var(_) => {}
            ExpressionWithoutType::Subscript(ref mut src, ref mut inner) => {
                src.validate(context)?;
                inner.validate(context)?;
            }
            ExpressionWithoutType::Assignment(ref mut dst, ref mut src) => {
                dst.validate(context)?;
                src.validate(context)?;
            }
            ExpressionWithoutType::Ternary(ref mut condition, ref mut then, ref mut otherwise) => {
                condition.validate(context)?;
                then.validate(context)?;
                otherwise.validate(context)?;
            }
            ExpressionWithoutType::FunctionCall(_, ref mut params) => {
                params.validate(context)?;
            }
            ExpressionWithoutType::Cast(_, ref mut expression) => {
                expression.validate(context)?;
            }
            ExpressionWithoutType::Dereference(ref mut ptr) => {
                ptr.validate(context)?;
            }
            ExpressionWithoutType::AddressOf(ref mut object) => {
                object.validate(context)?;
            }
            ExpressionWithoutType::String(ref mut _s) => {}
        };
        Ok(())
    }
}

impl ExpressionNode {
    fn validate_lvalues(&mut self, _context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match &self.0 {
            ExpressionWithoutType::Unary(
                UnaryOperatorNode::PrefixIncrement
                | UnaryOperatorNode::PrefixDecrement
                | UnaryOperatorNode::SuffixIncrement
                | UnaryOperatorNode::SuffixDecrement,
                src,
            ) => {
                // VALIDATION: Make sure ++ and -- only operate on variables, not constants or
                // other expressions
                if !src.0.is_lvalue() {
                    return Err(format!(
                        "Can't perform increment/decrement operation on non-variable: {:?}",
                        src,
                    )
                    .into());
                }
            }
            ExpressionWithoutType::Assignment(ref dst, _) => {
                if !dst.0.is_lvalue() {
                    return Err(format!("Can't assign to non-variable: {:?}", dst).into());
                }
            }
            ExpressionWithoutType::Compound(_, ref left, _) => {
                if !left.0.is_lvalue() {
                    return Err(format!("Can't assign to non-variable: {:?}", left).into());
                }
            }
            _ => {}
        }
        Ok(())
    }

    // See System V ABI list of rules for how to reconcile types of various sizes
    // C Spec 6.3.1.8, Paragraph 1
    fn get_common_type(e1: &ExpressionNode, e2: &ExpressionNode) -> Type {
        let t1 = e1.1.as_ref().unwrap();
        let t2 = e2.1.as_ref().unwrap();

        if t1 == t2 {
            t1.clone()
        } else if t1 == &Type::Double || t2 == &Type::Double {
            Type::Double
        } else if t1.get_size() == t2.get_size() {
            if t1.is_signed() {
                t2.clone()
            } else {
                t1.clone()
            }
        } else if t1.get_size() > t2.get_size() {
            t1.clone()
        } else {
            t2.clone()
        }
    }

    fn get_common_pointer_type(
        e1: &ExpressionNode,
        e2: &ExpressionNode,
    ) -> Result<Type, Box<dyn Error>> {
        let t1 = e1.1.as_ref().unwrap();
        let t2 = e2.1.as_ref().unwrap();
        if t1 == t2 {
            Ok(t1.clone())
        } else if e1.equals_null_pointer() {
            Ok(t2.clone())
        } else if e2.equals_null_pointer() {
            Ok(t1.clone())
        } else {
            Err(format!(
                "Incompatible types for implicit pointer cast: {:?} and {:?}",
                e1, e2
            )
            .into())
        }
    }

    pub fn equals_null_pointer(&self) -> bool {
        matches!(
            self.0,
            ExpressionWithoutType::Constant(Constant::Integer(0))
                | ExpressionWithoutType::Constant(Constant::Long(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedInteger(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedLong(0))
        )
    }

    pub fn convert_type(&mut self, target: &Type) {
        if self.1.as_ref().unwrap() != target {
            *self = ExpressionNode(
                ExpressionWithoutType::Cast(target.clone(), Box::new(self.clone())),
                Some(target.clone()),
            )
        }
    }

    pub fn convert_type_by_assignment(&mut self, target: &Type) -> Result<(), Box<dyn Error>> {
        if matches!(target, Type::Array(_, _)) {
            return Err("Can't assign to an array type".into());
        }
        let t1 = self.1.as_ref().unwrap();
        if t1 != target {
            if (t1.is_arithmetic() && target.is_arithmetic())
                || (self.equals_null_pointer() && matches!(target, Type::Pointer(_)))
            {
                self.convert_type(target);
            } else {
                return Err(
                    format!("Can't convert {:?} to {:?} by assignment", t1, target).into(),
                    // format!("Can't convert {:?} to {:?} by assignment", self, target).into(),
                );
            }
        }
        Ok(())
    }

    // required because functions that are compiled by clang assume that the caller always pads
    // 1-byte args to 4 bytes.
    pub fn pad_single_byte_arg(&mut self) -> Result<(), Box<dyn Error>> {
        let t1 = self.1.as_ref().unwrap();
        match t1 {
            Type::Char | Type::SignedChar => self.convert_type(&Type::Integer),
            Type::UnsignedChar => self.convert_type(&Type::UnsignedInteger),
            _ => {}
        }
        Ok(())
    }

    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if self.1.is_some() {
            // no need to type check this expression a second time
            return Ok(());
        }
        self.1 = Some(match self.0 {
            ExpressionWithoutType::FunctionCall(ref name, ref mut args) => {
                let type_info = context
                    .symbols
                    .get(name)
                    .expect("Function should have been defined")
                    .clone();
                if let Type::Function(out, params) = &type_info.symbol_type {
                    if params.len() != args.len() {
                        return Err("Function call has the wrong number of arguments".into());
                    }
                    for (arg, param) in args.iter_mut().zip(params.iter()) {
                        arg.check_types_and_convert(context)?;
                        arg.convert_type_by_assignment(param)?;
                        arg.pad_single_byte_arg()?;
                    }

                    *out.clone()
                } else {
                    return Err("Variable has been called as a function".into());
                }
            }
            ExpressionWithoutType::Var(ref name) => {
                let type_info = context
                    .symbols
                    .get(name)
                    .expect("Var should have been defined");

                match &type_info.symbol_type {
                    Type::Function(_, _) => {
                        return Err("Function has been defined as a variable".into())
                    }
                    t => t.clone(),
                }
            }
            ExpressionWithoutType::Constant(ref c) => match c {
                Constant::Integer(_) => Type::Integer,
                Constant::Long(_) => Type::Long,
                Constant::UnsignedInteger(_) => Type::UnsignedInteger,
                Constant::UnsignedLong(_) => Type::UnsignedLong,
                Constant::Double(_) => Type::Double,
                Constant::Char(_) => Type::Char,
                Constant::UnsignedChar(_) => Type::UnsignedChar,
            },
            ExpressionWithoutType::Unary(ref op, ref mut src) => {
                src.check_types_and_convert(context)?;
                if matches!(
                    op,
                    UnaryOperatorNode::Negate | UnaryOperatorNode::Complement
                ) {
                    if matches!(src.1.as_ref().unwrap(), Type::Pointer(_)) {
                        return Err("Can't apply - or ~ to a pointer".into());
                    }
                    // promote char types to int for this operation
                    if src.1.as_ref().unwrap().is_character() {
                        src.convert_type(&Type::Integer)
                    }
                }
                match op {
                    UnaryOperatorNode::Not => Type::Integer, // returns 0 or 1 (eg boolean-like)
                    UnaryOperatorNode::Complement => {
                        if src.1.as_ref().unwrap() == &Type::Double {
                            return Err("'~' cannot operate on a double".into());
                        }
                        src.1.clone().unwrap()
                    }
                    _ => src.1.clone().unwrap(),
                }
            }
            ExpressionWithoutType::Compound(ref op, ref mut left, ref mut right) => {
                left.check_types_and_convert(context)?;
                right.check_types_and_convert(context)?;
                let mut left_clone = left.clone();
                left_clone.promote(context)?;
                right.promote(context)?;
                // undo type conversion on Left, it will be done manually in the birds generation
                // stage
                let common_type = ExpressionNode::check_types_binary(op, &mut left_clone, right)?;
                // verify that it is possible to assign from the common type to left, without
                // actually doing the conversion yet.
                left_clone.convert_type_by_assignment(&common_type)?;
                common_type
            }
            ExpressionWithoutType::Binary(ref op, ref mut left, ref mut right) => {
                left.check_types_and_convert(context)?;
                right.check_types_and_convert(context)?;
                left.promote(context)?;
                right.promote(context)?;
                ExpressionNode::check_types_binary(op, left, right)?
            }
            ExpressionWithoutType::Assignment(ref mut dst, ref mut src) => {
                dst.check_types_and_convert(context)?;
                src.check_types_and_convert(context)?;
                src.convert_type_by_assignment(dst.1.as_ref().unwrap())?;
                dst.1.clone().unwrap()
            }
            ExpressionWithoutType::Ternary(ref mut cond, ref mut then, ref mut other) => {
                cond.check_types_and_convert(context)?;
                then.check_types_and_convert(context)?;
                other.check_types_and_convert(context)?;
                let common_type = if matches!(then.1.as_ref().unwrap(), Type::Pointer(_))
                    || matches!(other.1.as_ref().unwrap(), Type::Pointer(_))
                {
                    ExpressionNode::get_common_pointer_type(then, other)?
                } else {
                    ExpressionNode::get_common_type(then, other)
                };
                then.convert_type(&common_type);
                other.convert_type(&common_type);

                common_type
            }
            ExpressionWithoutType::Cast(ref target, ref mut e) => {
                e.check_types_and_convert(context)?;
                let src_type = e.1.as_ref().unwrap();
                if matches!((src_type, target), (Type::Double, Type::Pointer(_)))
                    || matches!((target, src_type), (Type::Double, Type::Pointer(_)))
                {
                    return Err("Cannot cast between Double and Pointer types".into());
                }

                if matches!(target, Type::Array(_, _)) {
                    return Err("Cannot cast to an Array type".into());
                }
                target.clone()
            }
            ExpressionWithoutType::Dereference(ref mut e) => {
                e.check_types_and_convert(context)?;
                if let Type::Pointer(t) = e.1.as_ref().unwrap() {
                    *t.clone()
                } else {
                    return Err("Dereference can only operate on pointer types".into());
                }
            }
            ExpressionWithoutType::AddressOf(ref mut e) => {
                // don't convert e to a pointer to the first element here, we want the full array
                e.check_types(context)?;
                if !e.0.is_lvalue() {
                    return Err("Can only take address of an object".into());
                }
                Type::Pointer(Box::new(e.1.clone().unwrap()))
            }
            ExpressionWithoutType::Subscript(ref mut src, ref mut inner) => {
                src.check_types_and_convert(context)?;
                inner.check_types_and_convert(context)?;
                let src_type = src.1.as_ref().unwrap();
                let inner_type = inner.1.as_ref().unwrap();

                if let Type::Pointer(left) = src_type {
                    if !inner_type.is_integer() {
                        return Err("Invalid subscript expression".into());
                    }
                    inner.convert_type(&Type::Long);
                    *left.clone()
                } else if let Type::Pointer(right) = inner_type {
                    if !src_type.is_integer() {
                        return Err("Invalid subscript expression".into());
                    }
                    src.convert_type(&Type::Long);
                    *right.clone()
                } else {
                    return Err("Invalid subscript expression".into());
                }
            }
            ExpressionWithoutType::String(ref s) => {
                // WEE WOO WEE WOO DON'T FORGET TO ASSIGN SPACE FOR THE NULL TERMINATOR
                Type::Array(Box::new(Type::Char), (s.len() as u64) + 1)
            }
        });

        Ok(())
    }

    pub fn check_types_and_convert(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        self.check_types(context)?;
        self.post_type_check_convert(context)?;

        Ok(())
    }

    fn post_type_check_convert(
        &mut self,
        _context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        // Extra expression inserted here to convert arrays to pointers
        if let Some(Type::Array(t, _)) = &self.1 {
            *self = ExpressionNode(
                ExpressionWithoutType::AddressOf(Box::new(self.clone())),
                Some(Type::Pointer(t.clone())),
            )
        }
        Ok(())
    }

    // promote char and unsigned char to signed int for all unary, binary and switch expressions
    pub fn promote(&mut self, _context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let t = self.1.as_ref().unwrap().promote().clone();
        self.convert_type(&t);

        Ok(())
    }

    fn check_types_binary(
        op: &BinaryOperatorNode,
        left: &mut ExpressionNode,
        right: &mut ExpressionNode,
    ) -> Result<Type, Box<dyn Error>> {
        let left_type = left.1.as_ref().unwrap();
        let right_type = right.1.as_ref().unwrap();

        // Get the common type of this expression
        // also handle ALL pointer arithmetic type-checking cases in this block
        let (common_type, is_pointer_arithmetic) =
            if (matches!(left_type, Type::Pointer(_)) || matches!(right_type, Type::Pointer(_))) {
                match op {
                    BinaryOperatorNode::Add => {
                        if matches!(left_type, Type::Pointer(_)) && right_type.is_integer() {
                            right.convert_type(&Type::Long);
                            (left_type.clone(), true)
                        } else if matches!(right_type, Type::Pointer(_)) && left_type.is_integer() {
                            left.convert_type(&Type::Long);
                            (right_type.clone(), true)
                        } else {
                            return Err("Invalid pointer addition".into());
                        }
                    }
                    BinaryOperatorNode::Subtract => {
                        if matches!(left_type, Type::Pointer(_)) && right_type.is_integer() {
                            right.convert_type(&Type::Long);
                            (left_type.clone(), true)
                        } else if left_type == right_type {
                            // typedef stuff: for <stddef.h>, this type should be annotated as ptrdiff_t
                            (Type::Long, true)
                        } else {
                            return Err("Invalid pointer subtraction".into());
                        }
                    }
                    BinaryOperatorNode::Less
                    | BinaryOperatorNode::Greater
                    | BinaryOperatorNode::LessEqual
                    | BinaryOperatorNode::GreaterEqual => {
                        if left_type == right_type {
                            (left_type.clone(), false)
                        } else {
                            return Err("Can't compare pointers with different types".into());
                        }
                    }
                    BinaryOperatorNode::Equal | BinaryOperatorNode::NotEqual => {
                        (ExpressionNode::get_common_pointer_type(left, right)?, false)
                    }
                    BinaryOperatorNode::And | BinaryOperatorNode::Or => (Type::Integer, false),
                    o => return Err(format!("Invalid pointer operation: {:?}", o).into()),
                }
            } else {
                (ExpressionNode::get_common_type(left, right), false)
            };

        // convert the left and right to the common type (with exceptions)
        if !is_pointer_arithmetic
            && !matches!(
                op,
                BinaryOperatorNode::ShiftLeft
                    | BinaryOperatorNode::ShiftRight
                    | BinaryOperatorNode::And
                    | BinaryOperatorNode::Or
            )
        {
            left.convert_type(&common_type);
            right.convert_type(&common_type);
        }

        // Misc. Validation of types and operators
        if (matches!(left.1.as_ref().unwrap(), Type::Double)
            && matches!(
                op,
                BinaryOperatorNode::BitwiseAnd
                    | BinaryOperatorNode::BitwiseXor
                    | BinaryOperatorNode::BitwiseOr
                    | BinaryOperatorNode::ShiftLeft
                    | BinaryOperatorNode::ShiftRight
                    | BinaryOperatorNode::Mod
            ))
            || (matches!(right.1.as_ref().unwrap(), Type::Double)
                && matches!(
                    op,
                    BinaryOperatorNode::Mod
                        | BinaryOperatorNode::ShiftLeft
                        | BinaryOperatorNode::ShiftRight
                ))
        {
            return Err("Incompatible types for this binary operation".into());
        }

        Ok(match op {
            BinaryOperatorNode::Add
            | BinaryOperatorNode::Subtract
            | BinaryOperatorNode::Multiply
            | BinaryOperatorNode::Divide
            | BinaryOperatorNode::BitwiseAnd
            | BinaryOperatorNode::BitwiseXor
            | BinaryOperatorNode::BitwiseOr => common_type,
            // types which don't convert the right type (TODO: why is Mod here?)
            BinaryOperatorNode::ShiftLeft
            | BinaryOperatorNode::ShiftRight
            | BinaryOperatorNode::Mod => left.1.clone().unwrap(),
            // logical
            BinaryOperatorNode::And
            | BinaryOperatorNode::Or
            | BinaryOperatorNode::Equal
            | BinaryOperatorNode::NotEqual
            | BinaryOperatorNode::Less
            | BinaryOperatorNode::Greater
            | BinaryOperatorNode::LessEqual
            | BinaryOperatorNode::GreaterEqual => Type::Integer,
        })
    }
}
