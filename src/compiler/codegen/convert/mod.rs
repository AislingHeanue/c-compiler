use crate::compiler::{
    birds::{
        BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsTopLevel,
        BirdsUnaryOperatorNode, BirdsValueNode,
    },
    types::{Constant, StaticInitialiser, Type},
};
use itertools::{process_results, Itertools};
use std::error::Error;

use super::{
    AssemblyType, BinaryOperator, ConditionCode, Convert, ConvertContext, ImmediateValue,
    Instruction, Operand, Program, Register, TopLevel, UnaryOperator, DOUBLE_PARAM_REGISTERS,
    FUNCTION_PARAM_REGISTERS,
};

fn create_static_constant(
    alignment: u32,
    value: StaticInitialiser,
    context: &mut ConvertContext,
) -> Operand {
    if let Some(key) = context
        .constants
        .iter()
        .find(|(_, v)| **v == (alignment, value.clone()))
        .map(|(k, _)| k)
    {
        return Operand::MockReg(key.clone());
    }

    let new_name = format!("constant_{}", context.constants.len() + 1);
    context
        .constants
        .insert(new_name.clone(), (alignment, value));

    Operand::MockReg(new_name)
}

struct Args {
    double: Vec<Operand>,
    integer: Vec<(Operand, AssemblyType)>,
    stack: Vec<(Operand, AssemblyType)>,
}

fn classify_function_args(
    values: Vec<BirdsValueNode>,
    context: &mut ConvertContext,
) -> Result<Args, Box<dyn Error>> {
    let mut args = Args {
        integer: Vec::new(),
        double: Vec::new(),
        stack: Vec::new(),
    };

    for v in values {
        let t = AssemblyType::infer(&v, context)?.0;
        if t == AssemblyType::Double {
            if args.double.len() < 8 {
                args.double.push(v.convert(context)?);
            } else {
                args.stack.push((v.convert(context)?, t));
            }
        } else if args.integer.len() < 6 {
            args.integer.push((v.convert(context)?, t));
        } else {
            args.stack.push((v.convert(context)?, t));
        }
    }

    Ok(args)
}

impl Convert<Program> for BirdsProgramNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Program, Box<dyn Error>> {
        let mut body = self.body.convert(context)?;

        for (name, (align, value)) in context.constants.iter() {
            body.push(TopLevel::StaticConstant(
                name.clone(),
                *align,
                value.clone(),
            ))
        }

        Ok(Program {
            body,
            displaying_context: None,
        })
    }
}

impl Convert<AssemblyType> for Type {
    fn convert(self, _context: &mut ConvertContext) -> Result<AssemblyType, Box<dyn Error>> {
        match self {
            Type::Integer => Ok(AssemblyType::Longword),
            Type::Long => Ok(AssemblyType::Quadword),
            Type::UnsignedInteger => Ok(AssemblyType::Longword),
            Type::UnsignedLong => Ok(AssemblyType::Quadword),
            Type::Double => Ok(AssemblyType::Double),
            Type::Pointer(_) => Ok(AssemblyType::Quadword),
            Type::Array(t, size) => {
                let assembly_t = (*t).convert(_context)?;
                let size = assembly_t.get_size() * size as u32;
                let alignment = if size < 16 {
                    assembly_t.get_alignment()
                } else {
                    16
                };
                Ok(AssemblyType::ByteArray(size, alignment))
            }
            Type::Function(_, _) => Err("Tried to convert a function type".into()),
            Type::Char | Type::SignedChar | Type::UnsignedChar => Ok(AssemblyType::Byte),
        }
    }
}

impl AssemblyType {
    pub fn infer(
        src: &BirdsValueNode,
        context: &mut ConvertContext,
    ) -> Result<(AssemblyType, bool), Box<dyn Error>> {
        // returns the assembly type and whether the value is a signed number
        Ok(match src {
            BirdsValueNode::Constant(Constant::Integer(_)) => (AssemblyType::Longword, true),
            BirdsValueNode::Constant(Constant::Long(_)) => (AssemblyType::Quadword, true),
            BirdsValueNode::Constant(Constant::UnsignedInteger(_)) => {
                (AssemblyType::Longword, false)
            }
            BirdsValueNode::Constant(Constant::UnsignedLong(_)) => (AssemblyType::Quadword, false),
            BirdsValueNode::Constant(Constant::Double(_)) => (AssemblyType::Double, true),
            BirdsValueNode::Constant(Constant::Char(_)) => (AssemblyType::Byte, true),
            BirdsValueNode::Constant(Constant::UnsignedChar(_)) => (AssemblyType::Byte, false),
            BirdsValueNode::Var(name) => {
                let var_type = context.symbols.get(name).unwrap().symbol_type.clone();
                let signed = var_type.is_signed();
                (var_type.convert(context)?, signed)
            }
        })
    }

    pub fn get_alignment(&self) -> u32 {
        match self {
            AssemblyType::Byte => 1,
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(_size, alignment) => *alignment,
        }
    }

    pub fn get_size(&self) -> u32 {
        match self {
            AssemblyType::Byte => 1,
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(size, _alignment) => *size,
        }
    }
}

impl<U, V> Convert<Vec<U>> for Vec<V>
where
    V: Convert<U>,
{
    fn convert(self, context: &mut ConvertContext) -> Result<Vec<U>, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|function| function.convert(context)),
            |iter| iter.collect(),
        )
    }
}

impl Convert<TopLevel> for BirdsTopLevel {
    fn convert(self, context: &mut ConvertContext) -> Result<TopLevel, Box<dyn Error>> {
        match self {
            BirdsTopLevel::Function(..) => self.convert_function(context),
            BirdsTopLevel::StaticVariable(t, name, init, global) => Ok(TopLevel::StaticVariable(
                name,
                global,
                t.convert(context)?.get_alignment(),
                init,
            )),
            BirdsTopLevel::StaticConstant(t, name, init) => Ok(TopLevel::StaticConstant(
                name,
                t.convert(context)?.get_alignment(),
                init,
            )),
        }
    }
}
impl BirdsTopLevel {
    fn convert_function(self, context: &mut ConvertContext) -> Result<TopLevel, Box<dyn Error>> {
        let (name, params, parsed_instructions, global) = match self {
            BirdsTopLevel::Function(a, b, c, d) => (a, b, c, d),
            _ => unreachable!(),
        };
        let params_vars = params
            .iter()
            .map(|name| BirdsValueNode::Var(name.to_string()))
            .collect_vec();

        let params = classify_function_args(params_vars, context)?;

        let mut instructions: Vec<Instruction> = Vec::new();

        // ZEROTH PASS: Copy all the params out of registers onto the stack.
        for (i, (param, param_type)) in params.integer.into_iter().enumerate() {
            instructions.push(Instruction::Mov(
                param_type,
                Operand::Reg(FUNCTION_PARAM_REGISTERS[i].clone()),
                param,
            ));
        }

        for (i, param) in params.double.into_iter().enumerate() {
            instructions.push(Instruction::Mov(
                AssemblyType::Double,
                Operand::Reg(DOUBLE_PARAM_REGISTERS[i].clone()),
                param,
            ));
        }

        for (i, (param, param_type)) in params.stack.into_iter().enumerate() {
            instructions.push(Instruction::Mov(
                param_type,
                Operand::Memory(Register::BP, 16 + 8 * (i as i32)),
                param,
            ));
        }

        // FIRST PASS: create a bunch of mock registers to be replaced with stack entries later
        instructions.append(&mut process_results(
            parsed_instructions
                .into_iter()
                .map(|instruction| instruction.convert(context)),
            |iter| iter.flatten().collect(),
        )?);

        Ok(TopLevel::Function(name, instructions, global))
    }
}

impl Convert<Vec<Instruction>> for BirdsInstructionNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Vec<Instruction>, Box<dyn Error>> {
        Ok(match self {
            BirdsInstructionNode::Return(src) => {
                let this_type = AssemblyType::infer(&src, context)?.0;
                if this_type == AssemblyType::Double {
                    vec![
                        Instruction::Mov(
                            this_type,
                            src.convert(context)?,
                            Operand::Reg(Register::XMM0),
                        ),
                        Instruction::Ret,
                    ]
                } else {
                    vec![
                        Instruction::Mov(
                            this_type,
                            src.convert(context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Ret,
                    ]
                }
            }
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
                    Instruction::SetCondition(ConditionCode::E, dst.convert(context)?),
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
                let (left_type, is_signed) = AssemblyType::infer(&left, context)?;
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
                let (left_type, is_signed) = AssemblyType::infer(&left, context)?;
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
                let (left_type, is_signed) = AssemblyType::infer(&left, context)?;
                // use different condition codes if the expression type is unsigned
                let instruction = if is_signed && left_type != AssemblyType::Double {
                    Instruction::SetCondition(op.convert(context)?, dst.clone().convert(context)?)
                } else {
                    Instruction::SetCondition(
                        op.convert_condition_unsigned(context)?,
                        dst.clone().convert(context)?,
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
                let (left_type, is_signed) = AssemblyType::infer(&left, context)?;
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
            BirdsInstructionNode::Copy(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                vec![Instruction::Mov(
                    src_type,
                    src.convert(context)?,
                    dst.convert(context)?,
                )]
            }
            BirdsInstructionNode::CopyToOffset(src, dst, offset) => {
                let t = AssemblyType::infer(&src, context)?.0;
                vec![Instruction::Mov(
                    t,
                    src.convert(context)?,
                    Operand::MockMemory(dst, offset),
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
                        Instruction::JmpCondition(ConditionCode::E, s),
                    ]
                } else {
                    vec![
                        Instruction::Cmp(
                            src_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            src.convert(context)?,
                        ),
                        Instruction::JmpCondition(ConditionCode::E, s),
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
                        Instruction::JmpCondition(ConditionCode::Ne, s),
                    ]
                } else {
                    vec![
                        Instruction::Cmp(
                            src_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            src.convert(context)?,
                        ),
                        Instruction::JmpCondition(ConditionCode::Ne, s),
                    ]
                }
            }
            BirdsInstructionNode::Label(s) => vec![Instruction::Label(s)],
            BirdsInstructionNode::FunctionCall(name, args, dst) => {
                let mut instructions = Vec::new();
                let args = classify_function_args(args, context)?;

                let return_type = if let Type::Function(ref ret, _) =
                    context.symbols.get(&name).unwrap().symbol_type
                {
                    ret.clone()
                } else {
                    return Err("Function is not listed as a function type".into());
                };

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
                    instructions.push(Instruction::Mov(
                        arg_type,
                        arg,
                        Operand::Reg(FUNCTION_PARAM_REGISTERS[i].clone()),
                    ));
                }

                for (i, arg) in args.double.into_iter().enumerate() {
                    instructions.push(Instruction::Mov(
                        AssemblyType::Double,
                        arg,
                        Operand::Reg(DOUBLE_PARAM_REGISTERS[i].clone()),
                    ));
                }

                for (arg, arg_type) in args.stack.into_iter().rev() {
                    if matches!(arg, Operand::Imm(_) | Operand::Reg(_))
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

                // read returned value from XMM0 if the return type is a double
                if *return_type == Type::Double {
                    instructions.push(Instruction::Mov(
                        AssemblyType::Double,
                        Operand::Reg(Register::XMM0),
                        dst.clone().convert(context)?,
                    ));
                } else {
                    instructions.push(Instruction::Mov(
                        (*return_type).convert(context)?,
                        Operand::Reg(Register::AX),
                        dst.clone().convert(context)?,
                    ));
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
                                Operand::Memory(Register::AX, index_value * scale),
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
                                Operand::Indexed(Register::AX, Register::DX, scale),
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
                                Operand::Imm(ImmediateValue::Signed(scale.into())),
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

impl Convert<Operand> for BirdsValueNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Operand, Box<dyn Error>> {
        match self {
            BirdsValueNode::Constant(Constant::Integer(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c as i64)))
            }
            BirdsValueNode::Constant(Constant::Long(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c)))
            }
            BirdsValueNode::Constant(Constant::UnsignedInteger(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c as u64)))
            }
            BirdsValueNode::Constant(Constant::UnsignedLong(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c)))
            }
            BirdsValueNode::Constant(Constant::Char(c)) => {
                Ok(Operand::Imm(ImmediateValue::Signed(c as i64)))
            }
            BirdsValueNode::Constant(Constant::UnsignedChar(c)) => {
                Ok(Operand::Imm(ImmediateValue::Unsigned(c as u64)))
            }
            BirdsValueNode::Constant(Constant::Double(c)) => Ok(create_static_constant(
                8,
                StaticInitialiser::Double(c),
                context,
            )),
            BirdsValueNode::Var(s) => {
                let var_type = &context.symbols.get(&s).unwrap().symbol_type;
                if var_type.is_scalar() {
                    Ok(Operand::MockReg(s))
                } else {
                    Ok(Operand::MockMemory(s, 0))
                }
            }
        }
    }
}

impl BirdsValueNode {
    fn get_constant_value(&self) -> Option<i32> {
        match self {
            BirdsValueNode::Constant(constant) => match constant {
                Constant::Integer(c) => Some(*c),
                Constant::Long(c) => Some((*c).try_into().unwrap()),
                Constant::UnsignedInteger(c) => Some((*c).try_into().unwrap()),
                Constant::UnsignedLong(c) => Some((*c).try_into().unwrap()),
                Constant::Double(_) => None,
                Constant::Char(c) => Some((*c).into()),
                Constant::UnsignedChar(c) => Some((*c).into()),
            },
            BirdsValueNode::Var(_) => None,
        }
    }
}

impl Convert<UnaryOperator> for BirdsUnaryOperatorNode {
    fn convert(self, _context: &mut ConvertContext) -> Result<UnaryOperator, Box<dyn Error>> {
        match self {
            BirdsUnaryOperatorNode::Negate => Ok(UnaryOperator::Neg),
            BirdsUnaryOperatorNode::Complement => Ok(UnaryOperator::Not),
            BirdsUnaryOperatorNode::Not => {
                panic!("Should not directly convert ! to a unary operator")
            }
        }
    }
}

impl Convert<BinaryOperator> for BirdsBinaryOperatorNode {
    fn convert(self, _context: &mut ConvertContext) -> Result<BinaryOperator, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::Add => Ok(BinaryOperator::Add),
            BirdsBinaryOperatorNode::Subtract => Ok(BinaryOperator::Sub),
            BirdsBinaryOperatorNode::Multiply => Ok(BinaryOperator::Mult),
            BirdsBinaryOperatorNode::BitwiseAnd => Ok(BinaryOperator::And),
            BirdsBinaryOperatorNode::BitwiseXor => Ok(BinaryOperator::Xor),
            BirdsBinaryOperatorNode::BitwiseOr => Ok(BinaryOperator::Or),
            BirdsBinaryOperatorNode::ShiftLeft => Ok(BinaryOperator::ShiftLeft),
            BirdsBinaryOperatorNode::ShiftRight => Ok(BinaryOperator::ShiftRight),
            BirdsBinaryOperatorNode::Divide => Ok(BinaryOperator::DivDouble),
            BirdsBinaryOperatorNode::Mod => {
                panic!("should not treat mod as binary expressions during codegen")
            }
            BirdsBinaryOperatorNode::Equal
            | BirdsBinaryOperatorNode::NotEqual
            | BirdsBinaryOperatorNode::Less
            | BirdsBinaryOperatorNode::Greater
            | BirdsBinaryOperatorNode::LessEqual
            | BirdsBinaryOperatorNode::GreaterEqual => {
                panic!("relational expressions should not be treated as binary expressions")
            }
        }
    }
}

impl BirdsBinaryOperatorNode {
    fn convert_unsigned(
        self,
        context: &mut ConvertContext,
    ) -> Result<BinaryOperator, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::ShiftLeft => Ok(BinaryOperator::UnsignedShiftLeft),
            BirdsBinaryOperatorNode::ShiftRight => Ok(BinaryOperator::UnsignedShiftRight),
            _ => self.convert(context),
        }
    }
}

impl Convert<ConditionCode> for BirdsBinaryOperatorNode {
    fn convert(self, _context: &mut ConvertContext) -> Result<ConditionCode, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::Equal => Ok(ConditionCode::E),
            BirdsBinaryOperatorNode::NotEqual => Ok(ConditionCode::Ne),
            BirdsBinaryOperatorNode::Less => Ok(ConditionCode::L),
            BirdsBinaryOperatorNode::Greater => Ok(ConditionCode::G),
            BirdsBinaryOperatorNode::LessEqual => Ok(ConditionCode::Le),
            BirdsBinaryOperatorNode::GreaterEqual => Ok(ConditionCode::Ge),
            _ if !self.is_relational() => {
                panic!("non-relational binary expressions should not be treated as relational expressions")
            }
            _ => unreachable!(),
        }
    }
}

impl BirdsBinaryOperatorNode {
    fn convert_condition_unsigned(
        self,
        _context: &mut ConvertContext,
    ) -> Result<ConditionCode, Box<dyn Error>> {
        match self {
            BirdsBinaryOperatorNode::Equal => Ok(ConditionCode::E),
            BirdsBinaryOperatorNode::NotEqual => Ok(ConditionCode::Ne),
            BirdsBinaryOperatorNode::Less => Ok(ConditionCode::B),
            BirdsBinaryOperatorNode::Greater => Ok(ConditionCode::A),
            BirdsBinaryOperatorNode::LessEqual => Ok(ConditionCode::Be),
            BirdsBinaryOperatorNode::GreaterEqual => Ok(ConditionCode::Ae),
            _ if !self.is_relational() => {
                panic!("non-relational binary expressions should not be treated as relational expressions")
            }
            _ => unreachable!(),
        }
    }
}
