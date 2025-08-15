use crate::compiler::{
    birds::{
        BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsUnaryOperatorNode, BirdsValueNode,
    },
    codegen::{
        convert::create_static_constant, AssemblyType, BinaryOperator, ConditionCode,
        ConvertContext, ImmediateValue, Instruction, Operand, Register, DOUBLE_PARAM_REGISTERS,
        DOUBLE_RETURN_REGISTERS, FUNCTION_PARAM_REGISTERS, FUNCTION_RETURN_REGISTERS,
    },
    types::StaticInitialiser,
};
use std::error::Error;

use super::{classify_function_args, classify_return, Convert, ReturnInfo};

impl Convert<Vec<Instruction>> for BirdsInstructionNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Vec<Instruction>, Box<dyn Error>> {
        Ok(match self {
            BirdsInstructionNode::Return(Some(src)) => {
                let returns = classify_return(src.clone(), context)?;
                let this_type = AssemblyType::infer(&src, context)?.0;
                let mut instructions = Vec::new();
                if returns.uses_memory {
                    instructions.push(Instruction::Mov(
                        AssemblyType::Quadword,
                        // read the location of where we're meant to store the return on the stack
                        Operand::Memory(Register::BP, -8),
                        Operand::Reg(Register::AX),
                    ));
                    let place_to_put_the_stuff = Operand::Memory(Register::AX, 0);
                    Instruction::copy_bytes(
                        this_type.get_size() as i32,
                        &mut instructions,
                        &src.convert(context)?,
                        &place_to_put_the_stuff,
                        0,
                        0,
                    )
                } else {
                    for (i, (op, t)) in returns.integer.iter().enumerate() {
                        if let AssemblyType::ByteArray(size, _) = t {
                            Instruction::copy_bytes_to_register(
                                *size as i32,
                                &mut instructions,
                                op,
                                &FUNCTION_RETURN_REGISTERS[i],
                            )
                        } else {
                            instructions.push(Instruction::Mov(
                                *t,
                                op.clone(),
                                Operand::Reg(FUNCTION_RETURN_REGISTERS[i].clone()),
                            ))
                        }
                    }

                    for (i, op) in returns.double.iter().enumerate() {
                        instructions.push(Instruction::Mov(
                            AssemblyType::Double,
                            op.clone(),
                            Operand::Reg(DOUBLE_RETURN_REGISTERS[i].clone()),
                        ))
                    }
                }
                instructions.push(Instruction::Ret);
                instructions
            }
            BirdsInstructionNode::Return(None) => vec![Instruction::Ret],
            BirdsInstructionNode::Unary(BirdsUnaryOperatorNode::Not, src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                vec![
                    if src_type == AssemblyType::Double {
                        let zero = create_static_constant(
                            src_type.get_alignment(),
                            StaticInitialiser::Double(0.),
                            context,
                        );
                        Instruction::Cmp(src_type, zero, src.convert(context)?)
                    } else {
                        Instruction::Cmp(
                            src_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            src.convert(context)?,
                        )
                    },
                    // Not returns 1 is src is zero, and 0 otherwise.
                    // zero out the destination (since SetCondition only affects the first byte).
                    Instruction::Mov(
                        dst_type,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        dst.clone().convert(context)?,
                    ),
                    Instruction::SetCondition(
                        ConditionCode::E,
                        dst.convert(context)?,
                        src_type == AssemblyType::Double,
                    ),
                ]
            }
            BirdsInstructionNode::Unary(op, src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                if src_type == AssemblyType::Double && op == BirdsUnaryOperatorNode::Negate {
                    let negative_zero =
                        create_static_constant(16, StaticInitialiser::Double(-0.0), context);
                    vec![
                        Instruction::Mov(
                            src_type,
                            src.convert(context)?,
                            dst.clone().convert(context)?,
                        ),
                        Instruction::Binary(
                            BinaryOperator::Xor,
                            src_type,
                            negative_zero,
                            dst.convert(context)?,
                        ),
                    ]
                } else {
                    vec![
                        Instruction::Mov(
                            src_type,
                            src.convert(context)?,
                            dst.clone().convert(context)?,
                        ),
                        Instruction::Unary(op.convert(context)?, src_type, dst.convert(context)?),
                    ]
                }
            }
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Divide, left, right, dst)
                if AssemblyType::infer(&left, context)?.0 != AssemblyType::Double =>
            {
                let (left_type, is_signed, _) = AssemblyType::infer(&left, context)?;
                if is_signed {
                    vec![
                        Instruction::Mov(
                            left_type,
                            left.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Cdq(left_type),
                        Instruction::Idiv(left_type, right.convert(context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::AX), // read quotient result from EAX
                            dst.convert(context)?,
                        ),
                    ]
                } else {
                    vec![
                        Instruction::Mov(
                            left_type,
                            left.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        // clear RDX with all zeros instead of whatever CDQ does.
                        Instruction::Mov(
                            left_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            Operand::Reg(Register::DX),
                        ),
                        Instruction::Div(left_type, right.convert(context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::AX), // read quotient result from EAX
                            dst.convert(context)?,
                        ),
                    ]
                }
            }
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Mod, left, right, dst) => {
                let (left_type, is_signed, _) = AssemblyType::infer(&left, context)?;
                if is_signed {
                    vec![
                        Instruction::Mov(
                            left_type,
                            left.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Cdq(left_type),
                        Instruction::Idiv(left_type, right.convert(context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::DX), // read remainder result from EDX
                            dst.convert(context)?,
                        ),
                    ]
                } else {
                    vec![
                        Instruction::Mov(
                            left_type,
                            left.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        // clear RDX with all zeros instead of whatever CDQ does.
                        Instruction::Mov(
                            left_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            Operand::Reg(Register::DX),
                        ),
                        Instruction::Div(left_type, right.convert(context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::DX), // read remainder result from EDX
                            dst.convert(context)?,
                        ),
                    ]
                }
            }
            BirdsInstructionNode::Binary(op, left, right, dst) if op.is_relational() => {
                let (left_type, is_signed, _) = AssemblyType::infer(&left, context)?;
                // use different condition codes if the expression type is unsigned
                let instruction = if is_signed && left_type != AssemblyType::Double {
                    Instruction::SetCondition(
                        op.convert(context)?,
                        dst.clone().convert(context)?,
                        false,
                    )
                } else {
                    Instruction::SetCondition(
                        op.convert_condition_unsigned(context)?,
                        dst.clone().convert(context)?,
                        left_type == AssemblyType::Double,
                    )
                };

                vec![
                    Instruction::Cmp(left_type, right.convert(context)?, left.convert(context)?),
                    // zero out the destination (since SetCondition only affects the first byte).
                    Instruction::Mov(
                        // always returns an integer
                        AssemblyType::Longword,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        dst.convert(context)?,
                    ),
                    instruction,
                ]
            }
            BirdsInstructionNode::Binary(op, left, right, dst) => {
                let (left_type, is_signed, _) = AssemblyType::infer(&left, context)?;
                // NOTE: right and left operands implicitly swap places here.
                // This is 100% expected because the 'left' operand in a binary
                // gets put into dst. Eg 1 - 2 turns into Sub(2, 1) and the result is
                // read from where 1 is.
                if left == dst {
                    vec![Instruction::Binary(
                        if is_signed {
                            op.convert(context)?
                        } else {
                            op.convert_unsigned(context)?
                        },
                        left_type,
                        right.convert(context)?,
                        dst.convert(context)?,
                    )]
                } else {
                    vec![
                        Instruction::Mov(
                            left_type,
                            left.convert(context)?,
                            dst.clone().convert(context)?,
                        ),
                        Instruction::Binary(
                            if is_signed {
                                op.convert(context)?
                            } else {
                                op.convert_unsigned(context)?
                            },
                            left_type,
                            right.convert(context)?,
                            dst.convert(context)?,
                        ),
                    ]
                }
            }
            BirdsInstructionNode::Copy(
                BirdsValueNode::Var(src_name),
                BirdsValueNode::Var(dst_name),
            ) => {
                let src_size = context
                    .symbols
                    .get(&src_name)
                    .unwrap()
                    .symbol_type
                    .clone()
                    .get_size(&mut context.structs) as i32;
                let mut instructions = Vec::new();
                Instruction::copy_bytes(
                    src_size,
                    &mut instructions,
                    &Operand::MockReg(src_name),
                    &Operand::MockReg(dst_name),
                    0,
                    0,
                );
                instructions
            }
            BirdsInstructionNode::Copy(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                vec![Instruction::Mov(
                    src_type,
                    src.convert(context)?,
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::CopyToOffset(BirdsValueNode::Var(src_name), dst, offset) => {
                let src_size = context
                    .symbols
                    .get(&src_name)
                    .unwrap()
                    .symbol_type
                    .clone()
                    .get_size(&mut context.structs) as i32;
                let mut instructions = Vec::new();
                Instruction::copy_bytes(
                    src_size,
                    &mut instructions,
                    &Operand::MockReg(src_name),
                    &Operand::MockReg(dst),
                    0,
                    offset,
                );
                instructions
            }
            BirdsInstructionNode::CopyToOffset(src, dst, offset) => {
                let t = AssemblyType::infer(&src, context)?.0;
                vec![Instruction::Mov(
                    t,
                    src.convert(context)?,
                    Operand::MockMemory(dst, offset),
                )]
            }
            BirdsInstructionNode::CopyFromOffset(src, offset, BirdsValueNode::Var(dst_name)) => {
                let dst_size = context
                    .symbols
                    .get(&dst_name)
                    .unwrap()
                    .symbol_type
                    .clone()
                    .get_size(&mut context.structs) as i32;
                let mut instructions = Vec::new();
                Instruction::copy_bytes(
                    dst_size,
                    &mut instructions,
                    &Operand::MockReg(src),
                    &Operand::MockReg(dst_name),
                    offset,
                    0,
                );
                instructions
            }
            BirdsInstructionNode::CopyFromOffset(src, offset, dst) => {
                let t = AssemblyType::infer(&dst, context)?.0;
                vec![Instruction::Mov(
                    t,
                    Operand::MockMemory(src, offset),
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::Jump(s) => vec![Instruction::Jmp(s)],
            BirdsInstructionNode::JumpZero(src, s) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                if src_type == AssemblyType::Double {
                    vec![
                        // Zero out XMM0 (this is the quickest way to get zero in double-land)
                        Instruction::Binary(
                            BinaryOperator::Xor,
                            src_type,
                            Operand::Reg(Register::XMM0),
                            Operand::Reg(Register::XMM0),
                        ),
                        Instruction::Cmp(
                            src_type,
                            Operand::Reg(Register::XMM0),
                            src.convert(context)?,
                        ),
                        Instruction::JmpCondition(ConditionCode::E, s, true),
                    ]
                } else {
                    vec![
                        Instruction::Cmp(
                            src_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            src.convert(context)?,
                        ),
                        Instruction::JmpCondition(ConditionCode::E, s, false),
                    ]
                }
            }
            BirdsInstructionNode::JumpNotZero(src, s) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                if src_type == AssemblyType::Double {
                    vec![
                        // Zero out XMM0 (this is the quickest way to get zero in double-land)
                        Instruction::Binary(
                            BinaryOperator::Xor,
                            src_type,
                            Operand::Reg(Register::XMM0),
                            Operand::Reg(Register::XMM0),
                        ),
                        Instruction::Cmp(
                            src_type,
                            Operand::Reg(Register::XMM0),
                            src.convert(context)?,
                        ),
                        Instruction::JmpCondition(ConditionCode::Ne, s, true),
                    ]
                } else {
                    vec![
                        Instruction::Cmp(
                            src_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            src.convert(context)?,
                        ),
                        Instruction::JmpCondition(ConditionCode::Ne, s, false),
                    ]
                }
            }
            BirdsInstructionNode::Label(s) => vec![Instruction::Label(s)],
            BirdsInstructionNode::FunctionCall(name, args, dst) => {
                let mut instructions = Vec::new();

                let returns = if let Some(ref d) = dst {
                    classify_return(d.clone(), context)?
                } else {
                    ReturnInfo {
                        integer: Vec::new(),
                        double: Vec::new(),
                        uses_memory: false,
                    }
                };

                let int_register_start = if returns.uses_memory {
                    instructions.push(Instruction::Lea(
                        dst.clone().unwrap().convert(context)?,
                        Operand::Reg(FUNCTION_PARAM_REGISTERS[0].clone()),
                    ));
                    1
                } else {
                    0
                };

                let args = classify_function_args(args, returns.uses_memory, context)?;

                let len_stack_args = args.stack.len();
                let stack_padding: i64 = if len_stack_args % 2 == 1 { 8 } else { 0 };
                if stack_padding != 0 {
                    instructions.push(Instruction::Binary(
                        BinaryOperator::Sub,
                        AssemblyType::Quadword,
                        Operand::Imm(ImmediateValue::Signed(stack_padding)),
                        Operand::Reg(Register::SP),
                    ));
                }

                for (i, (arg, arg_type)) in args.integer.into_iter().enumerate() {
                    match arg_type {
                        AssemblyType::ByteArray(size, _) => Instruction::copy_bytes_to_register(
                            size as i32,
                            &mut instructions,
                            &arg,
                            &FUNCTION_PARAM_REGISTERS[i + int_register_start],
                        ),
                        _ => {
                            instructions.push(Instruction::Mov(
                                arg_type,
                                arg,
                                Operand::Reg(
                                    FUNCTION_PARAM_REGISTERS[i + int_register_start].clone(),
                                ),
                            ));
                        }
                    }
                }

                for (i, arg) in args.double.into_iter().enumerate() {
                    instructions.push(Instruction::Mov(
                        AssemblyType::Double,
                        arg,
                        Operand::Reg(DOUBLE_PARAM_REGISTERS[i].clone()),
                    ));
                }

                for (arg, arg_type) in args.stack.into_iter().rev() {
                    if let AssemblyType::ByteArray(size, _) = arg_type {
                        instructions.push(Instruction::Binary(
                            BinaryOperator::Sub,
                            AssemblyType::Quadword,
                            Operand::Imm(ImmediateValue::Signed(8)),
                            Operand::Reg(Register::SP),
                        ));
                        Instruction::copy_bytes(
                            size as i32,
                            &mut instructions,
                            &arg,
                            &Operand::Memory(Register::SP, 0),
                            0,
                            0,
                        )
                    } else if matches!(arg, Operand::Imm(_) | Operand::Reg(_))
                        || matches!(arg_type, AssemblyType::Quadword | AssemblyType::Double)
                    {
                        instructions.push(Instruction::Push(arg));
                    } else {
                        instructions.push(Instruction::Mov(
                            arg_type,
                            arg,
                            Operand::Reg(Register::AX),
                        ));
                        instructions.push(Instruction::Push(Operand::Reg(Register::AX)));
                    }
                }

                instructions.push(Instruction::Call(name));

                let callee_stack_size: i64 = 8 * len_stack_args as i64 + stack_padding;
                if callee_stack_size != 0 {
                    instructions.push(Instruction::Binary(
                        BinaryOperator::Add,
                        AssemblyType::Quadword,
                        Operand::Imm(ImmediateValue::Signed(callee_stack_size)),
                        Operand::Reg(Register::SP),
                    ));
                }

                // the callee stuck its return values in an assortment of locations, collect them
                // into the locations listed in the returns struct (which corresponds to the real
                // dst).
                if dst.is_some() && !returns.uses_memory {
                    for (i, (op, t)) in returns.integer.iter().enumerate() {
                        if let AssemblyType::ByteArray(size, _) = t {
                            Instruction::copy_bytes_from_register(
                                *size as i32,
                                &mut instructions,
                                &FUNCTION_RETURN_REGISTERS[i],
                                op,
                            );
                        } else {
                            instructions.push(Instruction::Mov(
                                *t,
                                Operand::Reg(FUNCTION_RETURN_REGISTERS[i].clone()),
                                op.clone(),
                            ))
                        }
                    }
                    for (i, op) in returns.double.iter().enumerate() {
                        let this_src = Operand::Reg(DOUBLE_RETURN_REGISTERS[i].clone());
                        instructions.push(Instruction::Mov(
                            AssemblyType::Quadword,
                            this_src,
                            op.clone(),
                        ))
                    }
                }

                instructions
            }
            BirdsInstructionNode::SignedExtend(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                vec![Instruction::Movsx(
                    src_type,
                    dst_type,
                    src.convert(context)?,
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::Truncate(src, dst) => {
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                vec![Instruction::Mov(
                    dst_type,
                    src.convert(context)?,
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::ZeroExtend(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                vec![Instruction::MovZeroExtend(
                    src_type,
                    dst_type,
                    src.convert(context)?,
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::DoubleToInt(src, dst) => {
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                if dst_type != AssemblyType::Byte {
                    vec![Instruction::Cvttsd2si(
                        dst_type,
                        src.convert(context)?,
                        dst.convert(context)?,
                    )]
                } else {
                    vec![
                        Instruction::Cvttsd2si(
                            AssemblyType::Longword,
                            src.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Mov(
                            dst_type,
                            Operand::Reg(Register::AX),
                            dst.convert(context)?,
                        ),
                    ]
                }
            }
            BirdsInstructionNode::DoubleToUint(src, dst) => {
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                match dst_type {
                    AssemblyType::Quadword => {
                        context.num_labels += 1;
                        let instruction_label = format!("double_to_uint_{}", context.num_labels);
                        let i_max_value = i64::MAX as u64 + 1;
                        let i_max = create_static_constant(
                            8,
                            StaticInitialiser::Double(i_max_value as f64),
                            context,
                        );

                        vec![
                            Instruction::Cmp(
                                AssemblyType::Double,
                                i_max.clone(),
                                src.clone().convert(context)?,
                            ),
                            Instruction::JmpCondition(
                                ConditionCode::Ae,
                                format!("out_of_range_{}", &instruction_label),
                                true,
                            ),
                            Instruction::Cvttsd2si(
                                AssemblyType::Quadword,
                                src.clone().convert(context)?,
                                dst.clone().convert(context)?,
                            ),
                            Instruction::Jmp(format!("end_{}", &instruction_label)),
                            Instruction::Label(format!("out_of_range_{}", &instruction_label)),
                            // avoid overwriting src (eg. if it's a variable we want to reference
                            // later)
                            Instruction::Mov(
                                AssemblyType::Double,
                                src.convert(context)?,
                                Operand::Reg(Register::XMM0),
                            ),
                            Instruction::Binary(
                                BinaryOperator::Sub,
                                AssemblyType::Double,
                                i_max,
                                Operand::Reg(Register::XMM0),
                            ),
                            Instruction::Cvttsd2si(
                                AssemblyType::Quadword,
                                Operand::Reg(Register::XMM0),
                                dst.clone().convert(context)?,
                            ),
                            Instruction::Binary(
                                BinaryOperator::Add,
                                AssemblyType::Quadword,
                                Operand::Imm(ImmediateValue::Unsigned(i_max_value)),
                                dst.convert(context)?,
                            ),
                            Instruction::Label(format!("end_{}", &instruction_label)),
                        ]
                    }
                    AssemblyType::Longword => vec![
                        Instruction::Cvttsd2si(
                            AssemblyType::Quadword,
                            src.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Mov(
                            dst_type,
                            Operand::Reg(Register::AX),
                            dst.convert(context)?,
                        ),
                    ],
                    AssemblyType::Byte => vec![
                        Instruction::Cvttsd2si(
                            AssemblyType::Longword,
                            src.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Mov(
                            dst_type,
                            Operand::Reg(Register::AX),
                            dst.convert(context)?,
                        ),
                    ],
                    AssemblyType::Double => unreachable!(),
                    AssemblyType::ByteArray(_size, _align) => unreachable!(),
                }
            }
            BirdsInstructionNode::IntToDouble(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                if src_type == AssemblyType::Byte {
                    vec![
                        Instruction::Movsx(
                            src_type,
                            AssemblyType::Longword,
                            src.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Cvtsi2sd(
                            AssemblyType::Longword,
                            Operand::Reg(Register::AX),
                            dst.convert(context)?,
                        ),
                    ]
                } else {
                    vec![Instruction::Cvtsi2sd(
                        src_type,
                        src.convert(context)?,
                        dst.convert(context)?,
                    )]
                }
            }
            BirdsInstructionNode::UintToDouble(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                match src_type {
                    AssemblyType::Byte => {
                        vec![
                            Instruction::MovZeroExtend(
                                src_type,
                                AssemblyType::Longword,
                                src.convert(context)?,
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Cvtsi2sd(
                                AssemblyType::Longword,
                                Operand::Reg(Register::AX),
                                dst.convert(context)?,
                            ),
                        ]
                    }
                    AssemblyType::Longword => {
                        vec![
                            Instruction::MovZeroExtend(
                                src_type,
                                AssemblyType::Quadword,
                                src.convert(context)?,
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Cvtsi2sd(
                                AssemblyType::Quadword,
                                Operand::Reg(Register::AX),
                                dst.convert(context)?,
                            ),
                        ]
                    }
                    AssemblyType::Quadword => {
                        context.num_labels += 1;
                        let instruction_label = format!("uint_to_double_{}", context.num_labels);
                        vec![
                            Instruction::Cmp(
                                AssemblyType::Quadword,
                                Operand::Imm(ImmediateValue::Signed(0)),
                                src.clone().convert(context)?,
                            ),
                            Instruction::JmpCondition(
                                ConditionCode::L,
                                format!("out_of_range_{}", &instruction_label),
                                false,
                            ),
                            Instruction::Cvtsi2sd(
                                AssemblyType::Quadword,
                                src.clone().convert(context)?,
                                dst.clone().convert(context)?,
                            ),
                            Instruction::Jmp(format!("end_{}", &instruction_label)),
                            Instruction::Label(format!("out_of_range_{}", &instruction_label)),
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                src.convert(context)?,
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                Operand::Reg(Register::AX),
                                Operand::Reg(Register::DX),
                            ),
                            Instruction::Binary(
                                BinaryOperator::UnsignedShiftRight,
                                AssemblyType::Quadword,
                                Operand::Imm(ImmediateValue::Signed(1)),
                                Operand::Reg(Register::DX),
                            ),
                            Instruction::Binary(
                                BinaryOperator::And,
                                AssemblyType::Quadword,
                                Operand::Imm(ImmediateValue::Signed(1)),
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Binary(
                                BinaryOperator::Or,
                                AssemblyType::Quadword,
                                Operand::Reg(Register::AX),
                                Operand::Reg(Register::DX),
                            ),
                            Instruction::Cvtsi2sd(
                                AssemblyType::Quadword,
                                Operand::Reg(Register::DX),
                                dst.clone().convert(context)?,
                            ),
                            Instruction::Binary(
                                BinaryOperator::Add,
                                AssemblyType::Double,
                                dst.clone().convert(context)?,
                                dst.convert(context)?,
                            ),
                            Instruction::Label(format!("end_{}", &instruction_label)),
                        ]
                    }
                    AssemblyType::Double | AssemblyType::ByteArray(_, _) => unreachable!(),
                }
            }
            BirdsInstructionNode::GetAddress(src, dst) => {
                vec![Instruction::Lea(
                    src.convert(context)?,
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::LoadFromPointer(
                BirdsValueNode::Var(src_name),
                BirdsValueNode::Var(dst_name),
            ) => {
                let dst_size = context
                    .symbols
                    .get(&dst_name)
                    .unwrap()
                    .symbol_type
                    .clone()
                    .get_size(&mut context.structs) as i32;

                let mut instructions = vec![Instruction::Mov(
                    AssemblyType::Quadword,
                    // pointer types are scalars, no need to check
                    Operand::MockReg(src_name.clone()),
                    Operand::Reg(Register::AX),
                )];
                Instruction::copy_bytes(
                    dst_size,
                    &mut instructions,
                    &Operand::Memory(Register::AX, 0),
                    &Operand::MockReg(dst_name),
                    0,
                    0,
                );
                instructions
            }
            BirdsInstructionNode::LoadFromPointer(src, dst) => {
                let t = AssemblyType::infer(&dst, context)?.0;
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        src.convert(context)?,
                        Operand::Reg(Register::AX),
                    ),
                    Instruction::Mov(t, Operand::Memory(Register::AX, 0), dst.convert(context)?),
                ]
            }
            BirdsInstructionNode::StoreInPointer(
                BirdsValueNode::Var(src_name),
                BirdsValueNode::Var(dst_name),
            ) => {
                let src_size = context
                    .symbols
                    .get(&src_name)
                    .unwrap()
                    .symbol_type
                    .clone()
                    .get_size(&mut context.structs) as i32;

                let mut instructions = vec![Instruction::Mov(
                    AssemblyType::Quadword,
                    // pointer types are scalars, no need to check
                    Operand::MockReg(dst_name.clone()),
                    Operand::Reg(Register::AX),
                )];
                Instruction::copy_bytes(
                    src_size,
                    &mut instructions,
                    &Operand::MockReg(src_name),
                    &Operand::Memory(Register::AX, 0),
                    0,
                    0,
                );
                instructions
            }
            BirdsInstructionNode::StoreInPointer(src, dst) => {
                let t = AssemblyType::infer(&src, context)?.0;
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        dst.convert(context)?,
                        Operand::Reg(Register::AX),
                    ),
                    Instruction::Mov(t, src.convert(context)?, Operand::Memory(Register::AX, 0)),
                ]
            }
            BirdsInstructionNode::AddPointer(ptr, index, scale, dst) => {
                match (scale, index.get_constant_value()) {
                    (_, Some(index_value)) => {
                        vec![
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                ptr.convert(context)?,
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Lea(
                                Operand::Memory(Register::AX, index_value * scale as i32),
                                dst.convert(context)?,
                            ),
                        ]
                    }
                    (1 | 2 | 4 | 8, _) => {
                        vec![
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                ptr.convert(context)?,
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                index.convert(context)?,
                                Operand::Reg(Register::DX),
                            ),
                            Instruction::Lea(
                                Operand::Indexed(Register::AX, Register::DX, scale as i32),
                                dst.convert(context)?,
                            ),
                        ]
                    }

                    _ => {
                        vec![
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                ptr.convert(context)?,
                                Operand::Reg(Register::AX),
                            ),
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                index.convert(context)?,
                                Operand::Reg(Register::DX),
                            ),
                            Instruction::Binary(
                                BinaryOperator::Mult,
                                AssemblyType::Quadword,
                                Operand::Imm(ImmediateValue::Unsigned(scale)),
                                Operand::Reg(Register::DX),
                            ),
                            Instruction::Lea(
                                Operand::Indexed(Register::AX, Register::DX, 1),
                                dst.convert(context)?,
                            ),
                        ]
                    }
                }
            }
        })
    }
}

impl Instruction {
    pub fn copy_bytes(
        size: i32,
        instructions: &mut Vec<Instruction>,
        src: &Operand,
        dst: &Operand,
        offset_src: i32,
        offset_dst: i32,
    ) {
        let mut i = 0;
        while i < size {
            if i + 8 <= size {
                instructions.push(Instruction::Mov(
                    AssemblyType::Quadword,
                    src.offset(i + offset_src),
                    dst.offset(i + offset_dst),
                ));
                i += 8;
            } else if i + 4 <= size {
                instructions.push(Instruction::Mov(
                    AssemblyType::Longword,
                    src.offset(i + offset_src),
                    dst.offset(i + offset_dst),
                ));
                i += 4;
            } else {
                instructions.push(Instruction::Mov(
                    AssemblyType::Byte,
                    src.offset(i + offset_src),
                    dst.offset(i + offset_dst),
                ));
                i += 1;
            }
        }
    }

    pub fn copy_bytes_to_register(
        size: i32,
        instructions: &mut Vec<Instruction>,
        src: &Operand,
        dst: &Register,
    ) {
        for i in (0..size).rev() {
            instructions.push(Instruction::Mov(
                AssemblyType::Byte,
                src.offset(i),
                Operand::Reg(dst.clone()),
            ));
            if i > 0 {
                instructions.push(Instruction::Binary(
                    BinaryOperator::UnsignedShiftLeft,
                    AssemblyType::Quadword,
                    Operand::Imm(ImmediateValue::Signed(8)),
                    Operand::Reg(dst.clone()),
                ));
            }
        }
    }
    pub fn copy_bytes_from_register(
        size: i32,
        instructions: &mut Vec<Instruction>,
        src: &Register,
        dst: &Operand,
    ) {
        for i in 0..size {
            instructions.push(Instruction::Mov(
                AssemblyType::Byte,
                Operand::Reg(src.clone()),
                dst.offset(i),
            ));
            if i < size - 1 {
                instructions.push(Instruction::Binary(
                    BinaryOperator::UnsignedShiftRight,
                    AssemblyType::Quadword,
                    Operand::Imm(ImmediateValue::Signed(8)),
                    Operand::Reg(src.clone()),
                ));
            }
        }
    }
}

impl Operand {
    fn offset(&self, offset: i32) -> Operand {
        match self {
            Operand::MockReg(s) => Operand::MockMemory(s.to_string(), offset),
            Operand::MockMemory(s, other_offset) => {
                Operand::MockMemory(s.to_string(), offset + other_offset)
            }
            Operand::Data(s, other_offset) => Operand::Data(s.to_string(), offset + other_offset),
            Operand::Memory(r, other_offset) => Operand::Memory(r.clone(), offset + other_offset),

            Operand::Reg(_) => unreachable!(),
            Operand::Imm(_) => unreachable!(),
            Operand::Indexed(..) => unreachable!(),
        }
    }
}
