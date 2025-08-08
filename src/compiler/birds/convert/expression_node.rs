use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    birds::{
        BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsUnaryOperatorNode, BirdsValueNode,
        Destination,
    },
    parser::{BinaryOperatorNode, ExpressionNode, ExpressionWithoutType, UnaryOperatorNode},
    types::{
        ComparableStatic, Constant, InitialValue, StaticInitialiser, StorageInfo, SymbolInfo, Type,
    },
};

use super::{new_temp_variable, Convert, ConvertContext};

// short-hand for a type that will appear a lot in this file.
pub type D = (Vec<BirdsInstructionNode>, Destination);
// E for Evaluated
pub type E = (Vec<BirdsInstructionNode>, BirdsValueNode);
type Ve = (Vec<BirdsInstructionNode>, Vec<BirdsValueNode>);

impl Convert<(Vec<BirdsInstructionNode>, Destination)> for ExpressionNode {
    // outputs a list of instructions, and the location of the output of the instructions.

    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<(Vec<BirdsInstructionNode>, Destination), Box<dyn Error>> {
        match self.0 {
            ExpressionWithoutType::Constant(c) => {
                Ok((Vec::new(), Destination::Direct(BirdsValueNode::Constant(c))))
            }
            ExpressionWithoutType::Var(name) => {
                Ok((Vec::new(), Destination::Direct(BirdsValueNode::Var(name))))
            }
            ExpressionWithoutType::Assignment(left, right) => {
                let (mut instructions, new_dst): D = left.convert(context)?;
                let (mut instructions_from_src, new_src) = right.convert(context)?;
                instructions.append(&mut instructions_from_src);
                let (mut instructions_from_assign, returns) =
                    ExpressionWithoutType::assign(new_src, new_dst)?;
                instructions.append(&mut instructions_from_assign);
                Ok((instructions, returns))
            }
            ExpressionWithoutType::Unary(op, src)
                if matches!(
                    op,
                    UnaryOperatorNode::PrefixIncrement | UnaryOperatorNode::PrefixDecrement
                ) =>
            {
                let new_dst_type = src.1.clone().unwrap();

                // How to convert and evaluate an expression which may contain a dereference
                let (mut instructions, new_src): D = src.convert(context)?;
                let (mut instructions_from_evaluate, evaluated_src) =
                    new_src.clone().evaluate(&new_dst_type, context);
                instructions.append(&mut instructions_from_evaluate);

                let new_dst = new_temp_variable(&new_dst_type, context);
                if let Type::Pointer(ref t) = new_dst_type {
                    let constant = if op == UnaryOperatorNode::PrefixIncrement {
                        1
                    } else {
                        -1
                    };
                    instructions.push(BirdsInstructionNode::AddPointer(
                        evaluated_src.clone(),
                        BirdsValueNode::Constant(Constant::get_typed(
                            constant,
                            &Type::Pointer(t.clone()),
                        )),
                        t.get_size(),
                        new_dst.clone(),
                    ));
                } else {
                    let bird_op = if op == UnaryOperatorNode::PrefixIncrement {
                        BirdsBinaryOperatorNode::Add
                    } else {
                        BirdsBinaryOperatorNode::Subtract
                    };
                    if new_dst_type == Type::Double {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            evaluated_src.clone(),
                            BirdsValueNode::Constant(Constant::get_double(1.)),
                            new_dst.clone(),
                        ));
                    } else {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            evaluated_src.clone(),
                            BirdsValueNode::Constant(Constant::get_typed(1, &new_dst_type)),
                            new_dst.clone(),
                        ));
                    }
                }

                let (mut instructions_from_assign, _returns) =
                    ExpressionWithoutType::assign(new_dst.clone(), new_src)?;
                instructions.append(&mut instructions_from_assign);

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Unary(op, src)
                if matches!(
                    op,
                    UnaryOperatorNode::SuffixIncrement | UnaryOperatorNode::SuffixDecrement
                ) =>
            {
                let new_dst_type = src.1.clone().unwrap();

                let (mut instructions, new_src): D = src.convert(context)?;
                let (mut instructions_from_evaluate, evaluated_src) =
                    new_src.clone().evaluate(&new_dst_type, context);
                instructions.append(&mut instructions_from_evaluate);

                let new_dst = new_temp_variable(&new_dst_type, context);
                instructions.push(BirdsInstructionNode::Copy(
                    evaluated_src.clone(),
                    new_dst.clone(),
                ));

                if let Type::Pointer(ref t) = new_dst_type {
                    let constant = if op == UnaryOperatorNode::SuffixIncrement {
                        1
                    } else {
                        -1
                    };
                    instructions.push(BirdsInstructionNode::AddPointer(
                        new_dst.clone(),
                        BirdsValueNode::Constant(Constant::get_typed(
                            constant,
                            &Type::Pointer(t.clone()),
                        )),
                        t.get_size(),
                        evaluated_src.clone(),
                    ));
                } else {
                    let bird_op = if op == UnaryOperatorNode::SuffixIncrement {
                        BirdsBinaryOperatorNode::Add
                    } else {
                        BirdsBinaryOperatorNode::Subtract
                    };
                    if new_dst_type == Type::Double {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            new_dst.clone(),
                            BirdsValueNode::Constant(Constant::get_double(1.)),
                            evaluated_src.clone(),
                        ));
                    } else {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            new_dst.clone(),
                            BirdsValueNode::Constant(Constant::get_typed(1, &new_dst_type)),
                            evaluated_src.clone(),
                        ));
                    }
                }
                let (mut instructions_from_assign, _returns) =
                    ExpressionWithoutType::assign(evaluated_src.clone(), new_src)?;
                instructions.append(&mut instructions_from_assign);

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Unary(op, src) => {
                let src_type = src.1.clone().unwrap();
                let (mut instructions, new_src): E = src.convert(context)?;

                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                    UnaryOperatorNode::Not => BirdsUnaryOperatorNode::Not,
                    UnaryOperatorNode::PrefixIncrement
                    | UnaryOperatorNode::PrefixDecrement
                    | UnaryOperatorNode::SuffixIncrement
                    | UnaryOperatorNode::SuffixDecrement => unreachable!(),
                };

                let new_dst_type = if bird_op == BirdsUnaryOperatorNode::Not {
                    Type::Integer
                } else {
                    src_type
                };

                let new_dst = new_temp_variable(&new_dst_type, context);

                instructions.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Compound(op, left, right) => {
                // since this is an assignment, we've already carried out type checking for the
                // return of this expression in the validation step.
                //
                // Compound(Add, left, right) => {
                //      addq left, right
                //      movq right, left
                //      dst = left
                // }
                // a += 5l => a = int(long(a) + 5l)
                let left_type = left.1.clone().unwrap();
                let right_type = right.1.clone().unwrap();
                let common_type = self.1.clone().unwrap();

                let (mut instructions, new_left): D = left.convert(context)?;

                let (mut instructions_from_dereference, evaluated_left) =
                    new_left.clone().evaluate(&left_type, context);
                instructions.append(&mut instructions_from_dereference);

                let (mut instructions_from_right, new_right): E = right.convert(context)?;
                instructions.append(&mut instructions_from_right);

                let bird_op = op.convert(context)?;

                match (&bird_op, &left_type, &right_type) {
                    (BirdsBinaryOperatorNode::Add, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            evaluated_left,
                            new_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(new_dst, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
                    (BirdsBinaryOperatorNode::Subtract, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let negate_right = new_temp_variable(right_t, context);
                        instructions.push(BirdsInstructionNode::Unary(
                            BirdsUnaryOperatorNode::Negate,
                            new_right,
                            negate_right.clone(),
                        ));

                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            evaluated_left,
                            negate_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(new_dst, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
                    _ if common_type == left_type
                        || matches!(
                            bird_op,
                            BirdsBinaryOperatorNode::ShiftLeft
                                | BirdsBinaryOperatorNode::ShiftRight
                        ) =>
                    {
                        let new_dst = new_temp_variable(&left_type, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            evaluated_left,
                            new_right,
                            new_dst.clone(),
                        ));

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(new_dst, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
                    _ => {
                        let casted_left = new_temp_variable(&common_type, context);
                        let mut instructions_from_first_cast = ExpressionWithoutType::cast(
                            evaluated_left.clone(),
                            casted_left.clone(),
                            &left_type,
                            &common_type,
                        )?;
                        instructions.append(&mut instructions_from_first_cast);

                        let new_dst = new_temp_variable(&common_type, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            casted_left.clone(),
                            new_right,
                            new_dst.clone(),
                        ));
                        let casted_result = new_temp_variable(&left_type, context);
                        let mut instructions_from_second_cast = ExpressionWithoutType::cast(
                            new_dst.clone(),
                            casted_result.clone(),
                            &common_type,
                            &left_type,
                        )?;
                        instructions.append(&mut instructions_from_second_cast);

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(casted_result, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
                }
            }
            ExpressionWithoutType::Binary(op, left, right)
                if matches!(op, BinaryOperatorNode::Or | BinaryOperatorNode::And) =>
            {
                let dst = new_temp_variable(&Type::Integer, context);

                let (mut instructions, new_left): E = left.convert(context)?;
                let (mut instructions_from_right, new_right): E = right.convert(context)?;
                context.last_end_label_number += 1;
                let end_label_name = format!("end_{}", context.last_end_label_number);

                context.last_false_label_number += 1;
                let false_label_name = format!("false_{}", context.last_false_label_number);

                context.last_true_label_number += 1;
                let true_label_name = format!("true_{}", context.last_true_label_number);
                match op {
                    BinaryOperatorNode::Or => {
                        instructions.push(BirdsInstructionNode::JumpNotZero(
                            new_left,
                            true_label_name.clone(),
                        ));

                        instructions.append(&mut instructions_from_right);

                        instructions.append(&mut vec![
                            BirdsInstructionNode::JumpNotZero(new_right, true_label_name.clone()),
                            BirdsInstructionNode::Copy(
                                BirdsValueNode::Constant(Constant::Integer(0)),
                                dst.clone(),
                            ),
                            BirdsInstructionNode::Jump(end_label_name.clone()),
                            BirdsInstructionNode::Label(true_label_name),
                            BirdsInstructionNode::Copy(
                                BirdsValueNode::Constant(Constant::Integer(1)),
                                dst.clone(),
                            ),
                            BirdsInstructionNode::Label(end_label_name),
                        ]);
                    }
                    BinaryOperatorNode::And => {
                        instructions.push(BirdsInstructionNode::JumpZero(
                            new_left,
                            false_label_name.clone(),
                        ));

                        instructions.append(&mut instructions_from_right);

                        instructions.append(&mut vec![
                            BirdsInstructionNode::JumpZero(new_right, false_label_name.clone()),
                            BirdsInstructionNode::Copy(
                                BirdsValueNode::Constant(Constant::Integer(1)),
                                dst.clone(),
                            ),
                            BirdsInstructionNode::Jump(end_label_name.clone()),
                            BirdsInstructionNode::Label(false_label_name),
                            BirdsInstructionNode::Copy(
                                BirdsValueNode::Constant(Constant::Integer(0)),
                                dst.clone(),
                            ),
                            BirdsInstructionNode::Label(end_label_name),
                        ]);
                    }
                    _ => panic!("Unexpected binary operator found: {:?}", op),
                }

                Ok((instructions, dst.into()))
            }
            ExpressionWithoutType::Binary(op, left, right) => {
                let left_type = left.1.clone().unwrap();
                let right_type = right.1.clone().unwrap();

                let (mut instructions, new_left): E = left.convert(context)?;
                let (mut instructions_from_right, new_right): E = right.convert(context)?;
                instructions.append(&mut instructions_from_right);
                let bird_op = op.convert(context)?;

                match (&bird_op, &left_type, &right_type) {
                    (BirdsBinaryOperatorNode::Add, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            new_left,
                            new_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                    (BirdsBinaryOperatorNode::Add, left_t, Type::Pointer(ref right_t))
                        if left_t.is_integer() =>
                    {
                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            new_right,
                            new_left,
                            right_t.get_size(),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                    (BirdsBinaryOperatorNode::Subtract, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let negate_right = new_temp_variable(right_t, context);
                        instructions.push(BirdsInstructionNode::Unary(
                            BirdsUnaryOperatorNode::Negate,
                            new_right,
                            negate_right.clone(),
                        ));

                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            new_left,
                            negate_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                    (
                        BirdsBinaryOperatorNode::Subtract,
                        Type::Pointer(ref left_t),
                        Type::Pointer(_right_t),
                    ) => {
                        let diff = new_temp_variable(&Type::Long, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            BirdsBinaryOperatorNode::Subtract,
                            new_left,
                            new_right,
                            diff.clone(),
                        ));
                        // ptrdiff_t
                        let new_dst = new_temp_variable(&Type::Long, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            BirdsBinaryOperatorNode::Divide,
                            diff,
                            BirdsValueNode::Constant(Constant::get_typed(
                                left_t.get_size().into(),
                                &Type::Long,
                            )),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }

                    _ => {
                        let new_dst_type = if bird_op.is_relational() {
                            Type::Integer
                        } else {
                            left_type
                        };
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            new_left,
                            new_right,
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                }
            }
            ExpressionWithoutType::Ternary(condition, then, otherwise) => {
                context.last_end_label_number += 1;
                let new_end_label_name = format!("end_{}", context.last_end_label_number);
                context.last_else_label_number += 1;
                let new_else_label_name = format!("else_{}", context.last_else_label_number);

                let (mut instructions, new_cond): E = condition.convert(context)?;
                instructions.push(BirdsInstructionNode::JumpZero(
                    new_cond,
                    new_else_label_name.clone(),
                ));

                let new_dst_type = then.1.clone().unwrap();
                let (mut instructions_from_then, new_then): E = then.convert(context)?;
                let new_dst = new_temp_variable(&new_dst_type, context);

                instructions.append(&mut instructions_from_then);
                instructions.append(&mut vec![
                    BirdsInstructionNode::Copy(new_then.clone(), new_dst.clone()),
                    BirdsInstructionNode::Jump(new_end_label_name.clone()),
                    BirdsInstructionNode::Label(new_else_label_name),
                ]);
                let (mut instructions_from_other, new_other) = otherwise.convert(context)?;
                instructions.append(&mut instructions_from_other);
                instructions.push(BirdsInstructionNode::Copy(new_other, new_dst.clone()));
                instructions.push(BirdsInstructionNode::Label(new_end_label_name));

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::FunctionCall(name, args) => {
                let (mut instructions, values): Ve = args.convert(context)?;

                let new_dst_type = if let Type::Function(ref ret, _) =
                    context.symbols.get(&name).unwrap().symbol_type
                {
                    ret.as_ref().clone()
                } else {
                    return Err(
                        "Function stored in symbol table does not have function type".into(),
                    );
                };
                let new_dst = new_temp_variable(&new_dst_type, context);
                instructions.push(BirdsInstructionNode::FunctionCall(
                    name,
                    values,
                    new_dst.clone(),
                ));

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Cast(target_type, e) => {
                let this_type = e.1.clone().unwrap();

                let (mut instructions, new_src): E = e.convert(context)?;
                if target_type == this_type {
                    return Ok((instructions, new_src.into()));
                }
                let new_dst = new_temp_variable(&target_type, context);
                let mut instructions_from_cast = ExpressionWithoutType::cast(
                    new_src,
                    new_dst.clone(),
                    &this_type,
                    &target_type,
                )?;
                instructions.append(&mut instructions_from_cast);

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Dereference(src) => {
                let (instructions, new_src): E = src.convert(context)?;
                Ok((instructions, Destination::Dereference(new_src)))
            }
            ExpressionWithoutType::AddressOf(src) => {
                let (mut instructions, new_src) = src.convert(context)?;
                match new_src {
                    Destination::Direct(val) => {
                        let new_dst = new_temp_variable(self.1.as_ref().unwrap(), context);
                        instructions.push(BirdsInstructionNode::GetAddress(val, new_dst.clone()));
                        Ok((instructions, new_dst.into()))
                    }
                    Destination::Dereference(val) => Ok((instructions, val.into())),
                }
            }
            ExpressionWithoutType::Subscript(src, inner) => {
                let left_type = src.1.clone().unwrap();
                let right_type = inner.1.clone().unwrap();

                let (mut instructions, new_left): E = src.convert(context)?;
                let (mut instructions_from_right, new_right): E = inner.convert(context)?;

                let (inner_type, pointer_is_on_left) = if let Type::Pointer(t) = left_type {
                    (t, true)
                } else if let Type::Pointer(t) = right_type {
                    (t, false)
                } else {
                    unreachable!()
                };

                instructions.append(&mut instructions_from_right);
                let new_dst = new_temp_variable(&Type::Pointer(inner_type.clone()), context);
                if pointer_is_on_left {
                    instructions.push(BirdsInstructionNode::AddPointer(
                        new_left,
                        new_right,
                        inner_type.get_size(),
                        new_dst.clone(),
                    ));
                } else {
                    instructions.push(BirdsInstructionNode::AddPointer(
                        new_right,
                        new_left,
                        inner_type.get_size(),
                        new_dst.clone(),
                    ));
                }
                Ok((instructions, Destination::Dereference(new_dst)))
            }
            ExpressionWithoutType::String(s) => {
                // Strings in initialisers for char arrays do not use this code path
                context.num_block_strings += 1;

                let new_name = format!("string.block.{}", context.num_block_strings);
                context.symbols.insert(
                    new_name.clone(),
                    SymbolInfo {
                        symbol_type: self.1.unwrap(),
                        // null terminator has not been appended yet, this happens later
                        storage: StorageInfo::Static(
                            InitialValue::Initial(vec![StaticInitialiser::Ordinal(
                                ComparableStatic::String(s, true),
                            )]),
                            false,
                        ),
                    },
                );

                let new_dst = BirdsValueNode::Var(new_name);
                Ok((Vec::new(), new_dst.into()))
            }
        }
    }
}

impl ExpressionWithoutType {
    fn cast(
        src: BirdsValueNode,
        dst: BirdsValueNode,
        this_type: &Type,
        target_type: &Type,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        let mut instructions = Vec::new();
        if target_type == &Type::Double {
            match this_type {
                Type::Integer | Type::Long | Type::Char | Type::SignedChar => {
                    instructions.push(BirdsInstructionNode::IntToDouble(src, dst.clone()));
                }
                Type::UnsignedInteger | Type::UnsignedLong | Type::UnsignedChar => {
                    instructions.push(BirdsInstructionNode::UintToDouble(src, dst.clone()));
                }
                Type::Array(..) => unreachable!(),
                Type::Pointer(_) => unreachable!(),
                Type::Double => unreachable!(),
                Type::Function(_, _) => unreachable!(),
            }
        } else if this_type == &Type::Double {
            match target_type {
                Type::Integer | Type::Long | Type::Char | Type::SignedChar => {
                    instructions.push(BirdsInstructionNode::DoubleToInt(src, dst.clone()));
                }
                Type::UnsignedInteger | Type::UnsignedLong | Type::UnsignedChar => {
                    instructions.push(BirdsInstructionNode::DoubleToUint(src, dst.clone()));
                }
                Type::Pointer(_) => unreachable!(),
                Type::Array(..) => unreachable!(),
                Type::Double => unreachable!(),
                Type::Function(_, _) => unreachable!(),
            }
        } else if target_type.get_size() == this_type.get_size() {
            // mov the old type into the new type directly
            // C casting behaviour basically ends up saying "never alter the
            // underlying binary unless to extend or truncate it", indirectly
            instructions.push(BirdsInstructionNode::Copy(src, dst.clone()));
        } else if target_type.get_size() < this_type.get_size() {
            instructions.push(BirdsInstructionNode::Truncate(src, dst.clone()));
        } else if this_type.is_signed() {
            instructions.push(BirdsInstructionNode::SignedExtend(src, dst.clone()));
        } else {
            instructions.push(BirdsInstructionNode::ZeroExtend(src, dst.clone()));
        }
        Ok(instructions)
    }

    fn assign(
        src: BirdsValueNode,
        dst: Destination,
    ) -> Result<(Vec<BirdsInstructionNode>, Destination), Box<dyn Error>> {
        let mut instructions = Vec::new();
        match dst {
            Destination::Direct(ref val) => {
                instructions.push(BirdsInstructionNode::Copy(src.clone(), val.clone()));
                Ok((instructions, dst.clone()))
            }
            Destination::Dereference(ref val) => {
                instructions.push(BirdsInstructionNode::StoreInPointer(
                    src.clone(),
                    val.clone(),
                ));
                // since we can't easily read the contained value of dst which is normally
                // returned by the expression, we return the value of src here instead,
                // since it (by definition) is what *dst equals to at this instant.
                Ok((instructions, src.into()))
            }
        }
    }
}

impl Convert<(Vec<BirdsInstructionNode>, Vec<Destination>)> for Vec<ExpressionNode> {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<(Vec<BirdsInstructionNode>, Vec<Destination>), Box<dyn Error>> {
        let results: Vec<(Vec<BirdsInstructionNode>, Destination)> =
            process_results(self.into_iter().map(|a| a.convert(context)), |iter| {
                iter.collect()
            })?;

        let values = results.iter().map(|a| a.1.clone()).collect();
        let instructions = results.into_iter().flat_map(|a| a.0).collect();

        Ok((instructions, values))
    }
}

impl Convert<(Vec<BirdsInstructionNode>, BirdsValueNode)> for ExpressionNode {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<(Vec<BirdsInstructionNode>, BirdsValueNode), Box<dyn Error>> {
        let target_type = self.1.clone().unwrap();
        let (mut instructions, new_src): (Vec<BirdsInstructionNode>, Destination) =
            self.convert(context)?;
        let (mut deref_instructions, new_src) = new_src.evaluate(&target_type, context);
        instructions.append(&mut deref_instructions);

        Ok((instructions, new_src))
    }
}

impl Convert<(Vec<BirdsInstructionNode>, Vec<BirdsValueNode>)> for Vec<ExpressionNode> {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<(Vec<BirdsInstructionNode>, Vec<BirdsValueNode>), Box<dyn Error>> {
        let results: Vec<(Vec<BirdsInstructionNode>, BirdsValueNode)> =
            process_results(self.into_iter().map(|a| a.convert(context)), |iter| {
                iter.collect()
            })?;

        let values = results.iter().map(|a| a.1.clone()).collect();
        let instructions = results.into_iter().flat_map(|a| a.0).collect();

        Ok((instructions, values))
    }
}
