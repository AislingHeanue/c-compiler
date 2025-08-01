use crate::compiler::{
    birds::{
        BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsTopLevel,
        BirdsUnaryOperatorNode, BirdsValueNode,
    },
    parser::{Constant, Type},
};
use itertools::{izip, process_results};
use std::{cmp::min, error::Error};

use super::{
    AssemblyType, BinaryOperator, ConditionCode, Convert, ConvertContext, ImmediateValue,
    Instruction, Operand, Program, Register, TopLevel, UnaryOperator, FUNCTION_PARAM_REGISTERS,
};

impl Convert for Program {
    type Input = BirdsProgramNode;
    type Output = Self;

    fn convert(
        parsed: BirdsProgramNode,
        context: &mut ConvertContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(Program {
            body: Vec::<TopLevel>::convert(parsed.body, context)?,
            displaying_context: None,
        })
    }
}

impl Convert for AssemblyType {
    type Input = Type;
    type Output = Self;

    fn convert(parsed: Type, _context: &mut ConvertContext) -> Result<Self, Box<dyn Error>> {
        match parsed {
            Type::Integer => Ok(AssemblyType::Longword),
            Type::Long => Ok(AssemblyType::Quadword),
            Type::UnsignedInteger => Ok(AssemblyType::Longword),
            Type::UnsignedLong => Ok(AssemblyType::Quadword),
            Type::Function(_, _) => Err("Tried to convert a function type".into()),
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
            BirdsValueNode::Var(name) => {
                let var_type = context.symbols.get(name).unwrap().symbol_type.clone();
                let signed = var_type.is_signed();
                (AssemblyType::convert(var_type, context)?, signed)
            }
        })
    }

    pub fn get_alignment(&self) -> i32 {
        match self {
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
        }
    }
}

impl<U, V> Convert for Vec<U>
where
    U: Convert<Input = V, Output = U>,
{
    type Input = Vec<V>;
    type Output = Self;

    fn convert(
        parsed: Self::Input,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>> {
        process_results(
            parsed
                .into_iter()
                .map(|function| U::convert(function, context)),
            |iter| iter.collect(),
        )
    }
}

impl Convert for TopLevel {
    type Input = BirdsTopLevel;
    type Output = Self;

    fn convert(
        parsed: Self::Input,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>> {
        match parsed {
            BirdsTopLevel::Function(..) => Self::convert_function(parsed, context),
            BirdsTopLevel::StaticVariable(t, name, init, global) => Ok(TopLevel::StaticVariable(
                name,
                global,
                AssemblyType::convert(t, context)?
                    .get_alignment()
                    .try_into()
                    .unwrap(),
                init,
            )),
        }
    }
}
impl TopLevel {
    fn convert_function(
        parsed: BirdsTopLevel,
        context: &mut ConvertContext,
    ) -> Result<TopLevel, Box<dyn Error>> {
        let (name, params, parsed_instructions, global) = match parsed {
            BirdsTopLevel::Function(a, b, c, d) => (a, b, c, d),
            _ => unreachable!(),
        };
        let param_types = if let Type::Function(_, ref param_types) =
            context.symbols.get(&name).unwrap().symbol_type
        {
            param_types.clone()
        } else {
            unreachable!()
        };

        let mut instructions: Vec<Instruction> = Vec::new();

        // ZEROTH PASS: Copy all the params out of registers onto the stack.
        for (i, param_name, t) in izip!(0.., params, param_types) {
            if i < 6 {
                instructions.push(Instruction::Mov(
                    AssemblyType::convert(t.clone(), context)?,
                    Operand::Reg(FUNCTION_PARAM_REGISTERS[i].clone()),
                    Operand::MockReg(param_name.to_string()),
                ));
            } else {
                let unsigned_i: i32 = i.try_into().unwrap();
                instructions.push(Instruction::Mov(
                    AssemblyType::convert(t.clone(), context)?,
                    Operand::Stack(8 * (unsigned_i - 4)), // +24 as in, this is copying from the CALLER's frame
                    Operand::MockReg(param_name.to_string()),
                ));
            }
        }

        // FIRST PASS: create a bunch of mock registers to be replaced with stack entries later
        instructions.append(&mut process_results(
            parsed_instructions
                .into_iter()
                .map(|instruction| Instruction::convert(instruction, context)),
            |iter| iter.flatten().collect(),
        )?);

        Ok(TopLevel::Function(name, instructions, global))
    }
}

impl Convert for Instruction {
    type Input = BirdsInstructionNode;
    type Output = Vec<Instruction>;

    fn convert(
        input: BirdsInstructionNode,
        context: &mut ConvertContext,
    ) -> Result<Vec<Instruction>, Box<dyn Error>> {
        Ok(match input {
            BirdsInstructionNode::Return(src) => {
                let this_type = AssemblyType::infer(&src, context)?.0;
                vec![
                    Instruction::Mov(
                        this_type,
                        Operand::convert(src, context)?,
                        Operand::Reg(Register::AX),
                    ),
                    Instruction::Ret,
                ]
            }
            BirdsInstructionNode::Unary(BirdsUnaryOperatorNode::Not, src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                let dst_type = AssemblyType::infer(&dst, context)?.0;
                vec![
                    // Not returns 1 is src is zero, and 0 otherwise.
                    Instruction::Cmp(
                        src_type,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        Operand::convert(src, context)?,
                    ),
                    // zero out the destination (since SetCondition only affects the first byte).
                    Instruction::Mov(
                        dst_type,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        Operand::convert(dst.clone(), context)?,
                    ),
                    Instruction::SetCondition(ConditionCode::E, Operand::convert(dst, context)?),
                ]
            }
            BirdsInstructionNode::Unary(op, src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                vec![
                    Instruction::Mov(
                        src_type,
                        Operand::convert(src, context)?,
                        Operand::convert(dst.clone(), context)?,
                    ),
                    Instruction::Unary(
                        UnaryOperator::convert(op, context)?,
                        src_type,
                        Operand::convert(dst, context)?,
                    ),
                ]
            }
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Divide, left, right, dst) => {
                let (left_type, is_signed) = AssemblyType::infer(&left, context)?;
                if is_signed {
                    vec![
                        Instruction::Mov(
                            left_type,
                            Operand::convert(left, context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Cdq(left_type),
                        Instruction::Idiv(left_type, Operand::convert(right, context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::AX), // read quotient result from EAX
                            Operand::convert(dst, context)?,
                        ),
                    ]
                } else {
                    vec![
                        Instruction::Mov(
                            left_type,
                            Operand::convert(left, context)?,
                            Operand::Reg(Register::AX),
                        ),
                        // clear RDX with all zeros instead of whatever CDQ does.
                        Instruction::Mov(
                            left_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            Operand::Reg(Register::DX),
                        ),
                        Instruction::Div(left_type, Operand::convert(right, context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::AX), // read quotient result from EAX
                            Operand::convert(dst, context)?,
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
                            Operand::convert(left, context)?,
                            Operand::Reg(Register::AX),
                        ),
                        Instruction::Cdq(left_type),
                        Instruction::Idiv(left_type, Operand::convert(right, context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::DX), // read remainder result from EDX
                            Operand::convert(dst, context)?,
                        ),
                    ]
                } else {
                    vec![
                        Instruction::Mov(
                            left_type,
                            Operand::convert(left, context)?,
                            Operand::Reg(Register::AX),
                        ),
                        // clear RDX with all zeros instead of whatever CDQ does.
                        Instruction::Mov(
                            left_type,
                            Operand::Imm(ImmediateValue::Signed(0)),
                            Operand::Reg(Register::DX),
                        ),
                        Instruction::Div(left_type, Operand::convert(right, context)?),
                        Instruction::Mov(
                            left_type,
                            Operand::Reg(Register::DX), // read remainder result from EDX
                            Operand::convert(dst, context)?,
                        ),
                    ]
                }
            }
            BirdsInstructionNode::Binary(op, left, right, dst) if op.is_relational() => {
                let (left_type, is_signed) = AssemblyType::infer(&left, context)?;
                // use different condition codes if the expression type is unsigned
                let instruction = if is_signed {
                    Instruction::SetCondition(
                        ConditionCode::convert(op, context)?,
                        Operand::convert(dst.clone(), context)?,
                    )
                } else {
                    Instruction::SetCondition(
                        ConditionCode::convert_unsigned(op, context)?,
                        Operand::convert(dst.clone(), context)?,
                    )
                };

                vec![
                    Instruction::Cmp(
                        left_type,
                        Operand::convert(right, context)?,
                        Operand::convert(left, context)?,
                    ),
                    // zero out the destination (since SetCondition only affects the first byte).
                    Instruction::Mov(
                        left_type,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        Operand::convert(dst, context)?,
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
                vec![
                    Instruction::Mov(
                        left_type,
                        Operand::convert(left, context)?,
                        Operand::convert(dst.clone(), context)?,
                    ),
                    Instruction::Binary(
                        if is_signed {
                            BinaryOperator::convert(op, context)?
                        } else {
                            BinaryOperator::convert_unsigned(op, context)?
                        },
                        left_type,
                        Operand::convert(right, context)?,
                        Operand::convert(dst, context)?,
                    ),
                ]
            }
            BirdsInstructionNode::Copy(src, dst) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                vec![Instruction::Mov(
                    src_type,
                    Operand::convert(src, context)?,
                    Operand::convert(dst, context)?,
                )]
            }
            BirdsInstructionNode::Jump(s) => vec![Instruction::Jmp(s)],
            BirdsInstructionNode::JumpZero(src, s) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                vec![
                    Instruction::Cmp(
                        src_type,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        Operand::convert(src, context)?,
                    ),
                    Instruction::JmpCondition(ConditionCode::E, s),
                ]
            }
            BirdsInstructionNode::JumpNotZero(src, s) => {
                let src_type = AssemblyType::infer(&src, context)?.0;
                vec![
                    Instruction::Cmp(
                        src_type,
                        Operand::Imm(ImmediateValue::Signed(0)),
                        Operand::convert(src, context)?,
                    ),
                    Instruction::JmpCondition(ConditionCode::Ne, s),
                ]
            }
            BirdsInstructionNode::Label(s) => vec![Instruction::Label(s)],
            BirdsInstructionNode::FunctionCall(name, args, dst) => {
                let mut instructions = Vec::new();
                let (register_args, stack_args) = split(args, 6);

                let return_type = if let Type::Function(ref ret, _) =
                    context.symbols.get(&name).unwrap().symbol_type
                {
                    ret.clone()
                } else {
                    return Err("Function is not listed as a function type".into());
                };

                let stack_padding: i64 = if stack_args.len() % 2 == 1 { 8 } else { 0 };
                if stack_padding != 0 {
                    instructions.push(Instruction::Binary(
                        BinaryOperator::Sub,
                        AssemblyType::Quadword,
                        Operand::Imm(ImmediateValue::Signed(stack_padding)),
                        Operand::Reg(Register::SP),
                    ));
                }

                for (i, arg) in register_args.iter().enumerate() {
                    let arg_type = AssemblyType::infer(arg, context)?.0;
                    instructions.push(Instruction::Mov(
                        arg_type,
                        Operand::convert(arg.clone(), context)?,
                        Operand::Reg(FUNCTION_PARAM_REGISTERS[i].clone()),
                    ));
                }

                for arg in stack_args.iter().rev() {
                    let converted_arg = Operand::convert(arg.clone(), context)?;
                    let arg_type = AssemblyType::infer(arg, context)?.0;

                    if matches!(converted_arg, Operand::Reg(_) | Operand::Imm(_))
                        || arg_type == AssemblyType::Quadword
                    {
                        instructions.push(Instruction::Push(converted_arg));
                    } else {
                        instructions.push(Instruction::Mov(
                            arg_type,
                            converted_arg.clone(),
                            Operand::Reg(Register::AX),
                        ));
                        instructions.push(Instruction::Push(Operand::Reg(Register::AX)));
                    }
                }

                instructions.push(Instruction::Call(name));

                let callee_stack_size: i64 = 8 * stack_args.len() as i64 + stack_padding;
                if callee_stack_size != 0 {
                    instructions.push(Instruction::Binary(
                        BinaryOperator::Add,
                        AssemblyType::Quadword,
                        Operand::Imm(ImmediateValue::Signed(callee_stack_size)),
                        Operand::Reg(Register::SP),
                    ));
                }

                instructions.push(Instruction::Mov(
                    AssemblyType::convert(*return_type, context)?,
                    Operand::Reg(Register::AX),
                    Operand::convert(dst.clone(), context)?,
                ));

                instructions
            }
            BirdsInstructionNode::SignedExtend(src, dst) => {
                vec![Instruction::Movsx(
                    Operand::convert(src, context)?,
                    Operand::convert(dst, context)?,
                )]
            }
            BirdsInstructionNode::Truncate(src, dst) => {
                vec![Instruction::Mov(
                    AssemblyType::Longword, // explicit truncation to int
                    Operand::convert(src, context)?,
                    Operand::convert(dst, context)?,
                )]
            }
            BirdsInstructionNode::ZeroExtend(src, dst) => {
                vec![Instruction::MovZeroExtend(
                    Operand::convert(src, context)?,
                    Operand::convert(dst, context)?,
                )]
            }
        })
    }
}

fn split<T>(vec: Vec<T>, index: usize) -> (Vec<T>, Vec<T>) {
    let mut right = vec;
    let left = right.drain(..min(index, right.len())).collect();
    (left, right)
}

impl Convert for Operand {
    type Input = BirdsValueNode;
    type Output = Self;

    fn convert(
        input: BirdsValueNode,
        _context: &mut ConvertContext,
    ) -> Result<Operand, Box<dyn Error>> {
        match input {
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
            BirdsValueNode::Var(s) => Ok(Operand::MockReg(s)),
        }
    }
}

impl Convert for UnaryOperator {
    type Input = BirdsUnaryOperatorNode;
    type Output = Self;

    fn convert(
        input: BirdsUnaryOperatorNode,
        _context: &mut ConvertContext,
    ) -> Result<UnaryOperator, Box<dyn Error>> {
        match input {
            BirdsUnaryOperatorNode::Negate => Ok(UnaryOperator::Neg),
            BirdsUnaryOperatorNode::Complement => Ok(UnaryOperator::Not),
            BirdsUnaryOperatorNode::Not => {
                panic!("Should not directly convert ! to a unary operator")
            }
        }
    }
}

impl Convert for BinaryOperator {
    type Input = BirdsBinaryOperatorNode;
    type Output = Self;

    fn convert(
        input: BirdsBinaryOperatorNode,
        _context: &mut ConvertContext,
    ) -> Result<BinaryOperator, Box<dyn Error>> {
        match input {
            BirdsBinaryOperatorNode::Add => Ok(BinaryOperator::Add),
            BirdsBinaryOperatorNode::Subtract => Ok(BinaryOperator::Sub),
            BirdsBinaryOperatorNode::Multiply => Ok(BinaryOperator::Mult),
            BirdsBinaryOperatorNode::BitwiseAnd => Ok(BinaryOperator::BitwiseAnd),
            BirdsBinaryOperatorNode::BitwiseXor => Ok(BinaryOperator::BitwiseXor),
            BirdsBinaryOperatorNode::BitwiseOr => Ok(BinaryOperator::BitwiseOr),
            BirdsBinaryOperatorNode::ShiftLeft => Ok(BinaryOperator::ShiftLeft),
            BirdsBinaryOperatorNode::ShiftRight => Ok(BinaryOperator::ShiftRight),
            BirdsBinaryOperatorNode::Divide | BirdsBinaryOperatorNode::Mod => {
                panic!("should not treat mod and divide as binary expressions during codegen")
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

impl BinaryOperator {
    fn convert_unsigned(
        input: BirdsBinaryOperatorNode,
        context: &mut ConvertContext,
    ) -> Result<BinaryOperator, Box<dyn Error>> {
        match input {
            BirdsBinaryOperatorNode::ShiftLeft => Ok(BinaryOperator::UnsignedShiftLeft),
            BirdsBinaryOperatorNode::ShiftRight => Ok(BinaryOperator::UnsignedShiftRight),
            _ => BinaryOperator::convert(input, context),
        }
    }
}

impl Convert for ConditionCode {
    type Input = BirdsBinaryOperatorNode;
    type Output = Self;

    fn convert(
        input: BirdsBinaryOperatorNode,
        _context: &mut ConvertContext,
    ) -> Result<ConditionCode, Box<dyn Error>> {
        match input {
            BirdsBinaryOperatorNode::Equal => Ok(ConditionCode::E),
            BirdsBinaryOperatorNode::NotEqual => Ok(ConditionCode::Ne),
            BirdsBinaryOperatorNode::Less => Ok(ConditionCode::L),
            BirdsBinaryOperatorNode::Greater => Ok(ConditionCode::G),
            BirdsBinaryOperatorNode::LessEqual => Ok(ConditionCode::Le),
            BirdsBinaryOperatorNode::GreaterEqual => Ok(ConditionCode::Ge),
            _ if !input.is_relational() => {
                panic!("non-relational binary expressions should not be treated as relational expressions")
            }
            _ => unreachable!(),
        }
    }
}

impl ConditionCode {
    fn convert_unsigned(
        input: BirdsBinaryOperatorNode,
        _context: &mut ConvertContext,
    ) -> Result<ConditionCode, Box<dyn Error>> {
        match input {
            BirdsBinaryOperatorNode::Equal => Ok(ConditionCode::E),
            BirdsBinaryOperatorNode::NotEqual => Ok(ConditionCode::Ne),
            BirdsBinaryOperatorNode::Less => Ok(ConditionCode::B),
            BirdsBinaryOperatorNode::Greater => Ok(ConditionCode::A),
            BirdsBinaryOperatorNode::LessEqual => Ok(ConditionCode::Be),
            BirdsBinaryOperatorNode::GreaterEqual => Ok(ConditionCode::Ae),
            _ if !input.is_relational() => {
                panic!("non-relational binary expressions should not be treated as relational expressions")
            }
            _ => unreachable!(),
        }
    }
}
