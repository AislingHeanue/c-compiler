use std::{collections::HashMap, error::Error};

use crate::compiler::{
    parser::{
        BinaryOperatorNode, ExpressionNode, ExpressionWithoutType, StructInfo, UnaryOperatorNode,
    },
    types::{Constant, FindMemberName, Type},
};

use super::{CheckTypes, Validate, ValidateContext, ValidationPass};

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
            ExpressionWithoutType::Compound(_, ref mut left, ref mut right, _) => {
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
            ExpressionWithoutType::IndirectFunctionCall(ref mut left, ref mut params) => {
                left.validate(context)?;
                params.validate(context)?;
            }
            ExpressionWithoutType::Cast(_, ref mut expression, _) => {
                expression.validate(context)?;
            }
            ExpressionWithoutType::Dereference(ref mut ptr) => {
                ptr.validate(context)?;
            }
            ExpressionWithoutType::AddressOf(ref mut object) => {
                object.validate(context)?;
            }
            ExpressionWithoutType::String(ref mut _s) => {}
            ExpressionWithoutType::SizeOf(ref mut e) => {
                e.validate(context)?;
            }
            ExpressionWithoutType::SizeOfType(ref _t, _) => {}
            ExpressionWithoutType::Dot(ref mut e, _) => {
                e.validate(context)?;
            }
            ExpressionWithoutType::Arrow(ref mut e, _) => {
                e.validate(context)?;
            }
        };
        Ok(())
    }
}
impl CheckTypes for ExpressionNode {
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
                match &type_info.symbol_type {
                    Type::Function(out, params) => {
                        if **out != Type::Void && !out.is_complete(&mut context.structs) {
                            return Err(
                                "Can't call a function with an incomplete return type".into()
                            );
                        }
                        if params.len() != args.len() {
                            return Err("Function call has the wrong number of arguments".into());
                        }
                        for (arg, param) in args.iter_mut().zip(params.iter()) {
                            arg.check_types_and_convert(context)?;
                            arg.convert_type_by_assignment(param, context)?;
                        }

                        *out.clone()
                    }
                    Type::Pointer(p, _) => {
                        if let Type::Function(ref out, ref params) = **p {
                            let mut named_var = ExpressionNode(
                                ExpressionWithoutType::Var(name.to_string()),
                                None,
                                false,
                            );
                            named_var.check_types_and_convert(context)?;

                            if **out != Type::Void && !out.is_complete(&mut context.structs) {
                                return Err(
                                    "Can't call a function with an incomplete return type".into()
                                );
                            }
                            if params.len() != args.len() {
                                return Err(
                                    "Function call has the wrong number of arguments".into()
                                );
                            }
                            for (arg, param) in args.iter_mut().zip(params.iter()) {
                                arg.check_types_and_convert(context)?;
                                arg.convert_type_by_assignment(param, context)?;
                            }

                            // relabel this expression as an INDIRECT function call if no functions
                            // with this name are in scope
                            self.0 = ExpressionWithoutType::IndirectFunctionCall(
                                Box::new(named_var),
                                args.clone(),
                            );
                            *out.clone()
                        } else {
                            return Err(
                                "Non-function pointer variable has been called as a function"
                                    .into(),
                            );
                        }
                    }
                    _ => return Err("Variable has been called as a function".into()),
                }
            }
            ExpressionWithoutType::IndirectFunctionCall(ref mut left, ref mut args) => {
                left.check_types_and_convert(context)?;
                if let Some(Type::Pointer(ref p, _)) = &left.1 {
                    if let Type::Function(ref out, ref params) = **p {
                        if **out != Type::Void && !out.is_complete(&mut context.structs) {
                            return Err(
                                "Can't call a function with an incomplete return type".into()
                            );
                        }
                        if params.len() != args.len() {
                            return Err("Function call has the wrong number of arguments".into());
                        }
                        for (arg, param) in args.iter_mut().zip(params.iter()) {
                            arg.check_types_and_convert(context)?;
                            arg.convert_type_by_assignment(param, context)?;
                        }

                        *out.clone()
                    } else {
                        return Err(
                            "Non-function pointer expression has been called as a function".into(),
                        );
                    }
                } else {
                    return Err("Non-pointer expression has been called as a function".into());
                }
            }
            ExpressionWithoutType::Var(ref name) => {
                let type_info = context.symbols.get(name);
                if let Some(info) = type_info {
                    self.2 = info.constant;
                    // match &type_info.symbol_type {
                    //     // Type::Function(_, _) => {
                    //     //     println!("{:?}", self);
                    //     //     return Err("Function has been defined as a variable".into());
                    //     // }
                    //     t => t.clone(),
                    // }
                    info.symbol_type.clone()
                } else if name.ends_with(".expecting.enum.member") {
                    let name = &name[..name.len() - 22];
                    if let Some(member) =
                        context.enum_names_in_scope.iter().find(|m| m.name == *name)
                    {
                        *self = ExpressionNode(
                            ExpressionWithoutType::Constant(Constant::Integer(member.init)),
                            Some(Type::Integer),
                            true,
                        );
                        self.1.clone().unwrap()
                    } else {
                        return Err(
                            format!("Variable '{}' is used before it is declared", name).into()
                        );
                    }
                } else {
                    return Err(format!("Variable '{}' is used before it is declared", name).into());
                }
            }
            ExpressionWithoutType::Constant(ref c) => {
                // marking that this is a constant
                self.2 = true;
                c.get_type()
            }

            ExpressionWithoutType::Unary(ref op, ref mut src) => {
                src.check_types_and_convert(context)?;
                if matches!(
                    op,
                    UnaryOperatorNode::Negate | UnaryOperatorNode::Complement
                ) {
                    if !src.1.as_ref().unwrap().is_arithmetic() {
                        return Err("Can't apply - or ~ to a non-arithmetic type".into());
                    }
                    // promote char types to int for this operation
                    if src.1.as_ref().unwrap().is_smaller_than_int() {
                        src.convert_type(&Type::Integer)
                    }
                }
                if matches!(op, UnaryOperatorNode::Not) && !src.1.as_ref().unwrap().is_scalar() {
                    return Err("Can't apply ! to a non-scalar type".into());
                }
                match op {
                    UnaryOperatorNode::Not => Type::Integer, // returns 0 or 1 (eg boolean-like)
                    UnaryOperatorNode::Complement => {
                        if src.1.as_ref().unwrap().is_float() {
                            return Err("'~' cannot operate on a float or double".into());
                        }
                        src.1.clone().unwrap()
                    }
                    _ => src.1.clone().unwrap(),
                }
            }
            ExpressionWithoutType::Compound(
                ref op,
                ref mut left,
                ref mut right,
                ref mut common_type,
            ) => {
                left.check_types_and_convert(context)?;
                right.check_types_and_convert(context)?;

                if left.2 {
                    return Err("Can't preform a compound operation on a constant".into());
                }

                let mut left_clone = left.clone();
                left_clone.promote(context)?;
                right.promote(context)?;
                // undo type conversion on Left, it will be done manually in the birds generation
                // stage
                let common =
                    ExpressionNode::check_types_binary(op, &mut left_clone, right, context)?;
                // verify that it is possible to assign from the common type to left, without
                // actually doing the conversion yet.
                left_clone.convert_type_by_assignment(&common, context)?;

                *common_type = Some(common);
                left.1.clone().unwrap()
            }
            ExpressionWithoutType::Binary(ref op, ref mut left, ref mut right) => {
                left.check_types_and_convert(context)?;
                right.check_types_and_convert(context)?;
                left.promote(context)?;
                right.promote(context)?;
                ExpressionNode::check_types_binary(op, left, right, context)?
            }
            ExpressionWithoutType::Assignment(ref mut dst, ref mut src) => {
                dst.check_types_and_convert(context)?;
                if dst.2 {
                    return Err("Cannot assign to a constant after initial declaration".into());
                }
                let original_enum_names_in_scope = context.enum_names_in_scope.clone();
                if let Some(Type::Enum(members)) = &dst.1 {
                    context.enum_names_in_scope.append(&mut members.clone())
                }
                src.check_types_and_convert(context)?;
                src.convert_type_by_assignment(dst.1.as_ref().unwrap(), context)?;

                context.enum_names_in_scope = original_enum_names_in_scope;

                dst.1.clone().unwrap()
            }
            ExpressionWithoutType::Ternary(ref mut cond, ref mut then, ref mut other) => {
                cond.check_types_and_convert(context)?;
                then.check_types_and_convert(context)?;
                other.check_types_and_convert(context)?;
                if !cond.1.as_ref().unwrap().is_scalar() {
                    return Err(
                        "Can't construct a ternary expression with a non-scalar condition".into(),
                    );
                }
                let then_type = then.1.as_ref().unwrap();
                let other_type = other.1.as_ref().unwrap();
                let common_type = if then_type == &Type::Void && other_type == &Type::Void {
                    Type::Void
                } else if then_type.is_arithmetic() && other_type.is_arithmetic() {
                    ExpressionNode::get_common_type(then, other, &mut context.structs)
                } else if matches!(then_type, Type::Pointer(_, _))
                    || matches!(other_type, Type::Pointer(_, _))
                {
                    ExpressionNode::get_common_pointer_type(then, other)?
                } else if matches!(then_type, Type::Struct(_, _))
                    || matches!(other_type, Type::Struct(_, _))
                {
                    if then_type == other_type {
                        then_type.clone()
                    } else {
                        return Err(
                            "Can't construct a ternary expression with different struct types"
                                .into(),
                        );
                    }
                } else {
                    return Err("Invalid types for ternary expression".into());
                };
                then.convert_type(&common_type);
                other.convert_type(&common_type);

                common_type
            }
            ExpressionWithoutType::Cast(ref mut target, ref mut e, ref mut struct_declarations) => {
                for s in struct_declarations.iter_mut() {
                    s.check_types(context)?;
                }
                e.check_types_and_convert(context)?;
                let src_type = e.1.as_ref().unwrap();
                if matches!((&src_type, &target), (&Type::Double, &Type::Pointer(_, _)))
                    || matches!((&target, &src_type), (&Type::Double, &Type::Pointer(_, _)))
                    || matches!((&src_type, &target), (&Type::Float, &Type::Pointer(_, _)))
                    || matches!((&target, &src_type), (&Type::Float, &Type::Pointer(_, _)))
                {
                    return Err("Cannot cast between Float or Double and Pointer types".into());
                }

                target.check_types(context)?;

                if *target == Type::Void {
                    target.clone()
                } else if !target.is_scalar() {
                    return Err("Cannot cast to a non-scalar type".into());
                } else if !src_type.is_scalar() {
                    return Err("Cannot cast from a non-scalar type".into());
                } else {
                    target.clone()
                }
            }
            ExpressionWithoutType::Dereference(ref mut e) => {
                e.check_types_and_convert(context)?;
                if let Type::Pointer(t, _) = e.1.as_ref().unwrap() {
                    // if !t.is_complete(&mut context.structs) {
                    //     return Err("Cannot dereference a pointer to an incomplete type".into());
                    // }
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
                Type::Pointer(Box::new(e.1.clone().unwrap()), false)
            }
            ExpressionWithoutType::Subscript(ref mut src, ref mut inner) => {
                src.check_types_and_convert(context)?;
                inner.check_types_and_convert(context)?;
                let src_type = src.1.as_ref().unwrap();
                let inner_type = inner.1.as_ref().unwrap();

                if let Type::Pointer(left, _) = src_type {
                    if !inner_type.is_integer() || !left.is_complete(&mut context.structs) {
                        return Err("Invalid subscript expression".into());
                    }
                    inner.convert_type(&Type::Long);
                    *left.clone()
                } else if let Type::Pointer(right, _) = inner_type {
                    if !src_type.is_integer() || !right.is_complete(&mut context.structs) {
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
                Type::Array(Box::new(Type::Char), Some((s.len() as u64) + 1))
            }
            ExpressionWithoutType::SizeOf(ref mut e) => {
                e.check_types(context)?;
                if !e.1.as_ref().unwrap().is_complete(&mut context.structs) {
                    return Err(
                        "Can't get the size of an expression returning an incomplete type".into(),
                    );
                }
                // size_t
                Type::UnsignedLong
            }
            ExpressionWithoutType::SizeOfType(ref mut t, ref mut struct_declarations) => {
                for s in struct_declarations.iter_mut() {
                    s.check_types(context)?;
                }
                t.check_types(context)?;
                if !t.is_complete(&mut context.structs) {
                    return Err("Can't get the size of an incomplete type".into());
                }
                // size_t
                Type::UnsignedLong
            }
            ExpressionWithoutType::Dot(ref mut e, ref mut name) => {
                e.check_types(context)?;
                if let Type::Struct(s_name, is_union) = e.1.as_ref().unwrap() {
                    let info = context.structs.get(s_name).unwrap().clone();
                    let maybe_member =
                        info.members
                            .find_name(name, *is_union, &mut context.structs);
                    if let Some((found_member, _)) = maybe_member {
                        found_member.member_type.clone()
                    } else {
                        return Err(
                            format!("Could not find entry {} in struct {}", name, s_name).into(),
                        );
                    }
                } else {
                    return Err("'.' can only operate on a struct".into());
                }
            }
            ExpressionWithoutType::Arrow(ref mut p, ref mut name) => {
                p.check_types_and_convert(context)?;
                if let Type::Pointer(p_type, _) = p.1.as_ref().unwrap() {
                    if let Type::Struct(ref s_name, is_union) = **p_type {
                        let info = context.structs.get(s_name).unwrap().clone();
                        let maybe_member =
                            info.members.find_name(name, is_union, &mut context.structs);
                        if let Some((found_member, _)) = maybe_member {
                            found_member.member_type.clone()
                        } else {
                            return Err(format!(
                                "Could not find entry {} in struct {}",
                                name, s_name
                            )
                            .into());
                        }
                    } else {
                        return Err("'->' can only operate on a pointer to a struct".into());
                    }
                } else {
                    return Err("'->' can only operate on a pointer to a struct".into());
                }
            }
        });

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
                if let ExpressionWithoutType::Unary(
                    UnaryOperatorNode::PrefixIncrement | UnaryOperatorNode::PrefixDecrement,
                    _,
                ) = &self.0
                {
                    if src.1.as_ref().unwrap() == &Type::Void {
                        return Err(format!(
                            "Can't perform increment/decrement operation on void variable: {:?}",
                            src,
                        )
                        .into());
                    }
                }
                // VALIDATION: Make sure ++ and -- only operate on variables, not constants or
                // other expressions, and not pointers to void
                if !src.0.is_lvalue()
                    || matches!(src.1.as_ref(), Some(Type::Pointer(t, _)) if **t == Type::Void)
                    || matches!(src.1.as_ref().unwrap(), Type::Struct(_, _))
                {
                    return Err(format!(
                        "Can't perform increment/decrement operation on non-arithmetic variable: {:?}",
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
            ExpressionWithoutType::Compound(_, ref left, _, _) => {
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
    pub fn get_common_type(
        e1: &ExpressionNode,
        e2: &ExpressionNode,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Type {
        let t1 = e1.1.as_ref().unwrap();
        let t2 = e2.1.as_ref().unwrap();

        if t1 == t2 {
            t1.clone()
        } else if t1.is_float() {
            if t2.is_float() {
                if t1.get_size_for_common_type(structs) > t2.get_size_for_common_type(structs) {
                    t1.clone()
                } else {
                    t2.clone()
                }
            } else {
                t1.clone()
            }
        } else if t2.is_float() {
            t2.clone()
        } else if t1.get_size_for_common_type(structs) == t2.get_size_for_common_type(structs) {
            if t1.is_signed() {
                t2.clone()
            } else {
                t1.clone()
            }
        } else if t1.get_size_for_common_type(structs) > t2.get_size_for_common_type(structs) {
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
        } else if let (Type::Pointer(p1, left_restricted), Type::Pointer(p2, right_restricted)) =
            (t1, t2)
        {
            if **p1 == Type::Void || **p2 == Type::Void {
                return Ok(Type::Pointer(
                    Box::new(Type::Void),
                    // the resulting pointer is only restricted if both pointers in this expression
                    // are (at least I think that's how it's meant to work)
                    *left_restricted && *right_restricted,
                ));
            } else {
                Err(format!(
                    "Incompatible types for implicit pointer cast: {:?} and {:?}",
                    e1, e2
                )
                .into())
            }
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
                | ExpressionWithoutType::Constant(Constant::LongLong(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedInteger(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedLong(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedLongLong(0))
        )
    }

    pub fn convert_type(&mut self, target: &Type) {
        if self.1.as_ref().unwrap() != target {
            *self = ExpressionNode(
                ExpressionWithoutType::Cast(target.clone(), Box::new(self.clone()), Vec::new()),
                Some(target.clone()),
                false,
            )
        }
    }

    pub fn convert_type_by_assignment(
        &mut self,
        target: &Type,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        if matches!(target, Type::Array(_, _)) {
            return Err("Can't assign to an array type".into());
        }
        if !target.is_complete(&mut context.structs) {
            return Err("Can't assign to an incomplete type".into());
        }
        if self.equals_null_pointer() && matches!(target, Type::Pointer(_, _)) {
            *self = ExpressionNode(
                ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                Some(target.clone()),
                false,
            );
            return Ok(());
        }
        let t1 = self.1.as_ref().unwrap();
        if !t1.equal_for_assignment(target) {
            if (t1.is_arithmetic() && target.is_arithmetic())
                || (matches!(t1, Type::Pointer(t,_) if **t == Type::Void)
                    && matches!(target, Type::Pointer(_, _)))
                || (matches!(target, Type::Pointer(t,_) if **t == Type::Void)
                    && matches!(t1, Type::Pointer(_, _)))
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

    pub fn check_types_and_convert(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        self.check_types(context)?;
        self.post_type_check_convert(context)?;

        Ok(())
    }

    // type decay occurs here
    fn post_type_check_convert(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        // Extra expression inserted here to convert arrays to pointers
        match &self.1 {
            Some(Type::Array(t, _)) => {
                *self = ExpressionNode(
                    ExpressionWithoutType::AddressOf(Box::new(self.clone())),
                    Some(Type::Pointer(t.clone(), false)),
                    false,
                )
            }
            Some(Type::Function(_, _)) => {
                *self = ExpressionNode(
                    ExpressionWithoutType::AddressOf(Box::new(self.clone())),
                    Some(Type::Pointer(Box::new(self.1.clone().unwrap()), false)),
                    false,
                )
            }
            Some(Type::Struct(name, is_union)) => {
                if !Type::Struct(name.clone(), *is_union).is_complete(&mut context.structs) {
                    return Err("Invalid use of incomplete struct".into());
                }
            }
            _ => (),
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
        context: &mut ValidateContext,
    ) -> Result<Type, Box<dyn Error>> {
        let left_type = left.1.as_ref().unwrap().clone();
        let right_type = right.1.as_ref().unwrap().clone();

        // Get the common type of this expression
        // also handle ALL pointer arithmetic type-checking cases in this block
        let (common_type, is_pointer_arithmetic) = if (matches!(left_type, Type::Pointer(_, _))
            || matches!(right_type, Type::Pointer(_, _)))
        {
            match op {
                BinaryOperatorNode::Add => {
                    if left_type.is_complete_pointer(&mut context.structs)
                        && right_type.is_integer()
                    {
                        right.convert_type(&Type::Long);
                        (left_type.clone(), true)
                    } else if right_type.is_complete_pointer(&mut context.structs)
                        && left_type.is_integer()
                    {
                        left.convert_type(&Type::Long);
                        (right_type.clone(), true)
                    } else {
                        return Err("Invalid pointer addition".into());
                    }
                }
                BinaryOperatorNode::Subtract => {
                    if left_type.is_complete_pointer(&mut context.structs)
                        && right_type.is_integer()
                    {
                        right.convert_type(&Type::Long);
                        (left_type.clone(), true)
                    } else if left_type == right_type
                        && left_type.is_complete_pointer(&mut context.structs)
                    {
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
        } else if left_type.is_arithmetic() && right_type.is_arithmetic() {
            (
                ExpressionNode::get_common_type(left, right, &mut context.structs),
                false,
            )
        } else {
            return Err(format!(
                "Cannot perform binary operation on non-arithmetic types {:?} and {:?}",
                left_type, right_type
            )
            .into());
        };

        if (!left_type.is_scalar() || !right_type.is_scalar())
            && matches!(op, BinaryOperatorNode::And | BinaryOperatorNode::Or)
        {
            return Err(format!("Invalid pointer operation on non-scalars: {:?}", op).into());
        }

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
        if (left.1.as_ref().unwrap().is_float()
            && matches!(
                op,
                BinaryOperatorNode::BitwiseAnd
                    | BinaryOperatorNode::BitwiseXor
                    | BinaryOperatorNode::BitwiseOr
                    | BinaryOperatorNode::ShiftLeft
                    | BinaryOperatorNode::ShiftRight
                    | BinaryOperatorNode::Mod
            ))
            || (right_type.is_float()
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
            | BinaryOperatorNode::Mod
            | BinaryOperatorNode::BitwiseOr => common_type,
            // types which don't convert the right type
            BinaryOperatorNode::ShiftLeft | BinaryOperatorNode::ShiftRight => {
                left.1.clone().unwrap()
            }
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
