use crate::compiler::{
    birds::{
        BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsTopLevel,
        BirdsUnaryOperatorNode, BirdsValueNode,
    },
    parser::{StorageInfo, SymbolInfo},
};
use itertools::process_results;
use std::{cmp::min, collections::HashMap, error::Error};

use super::{
    BinaryOperator, ConditionCode, Convert, ConvertContext, DisplayContext, Instruction, Operand,
    Program, Register, TopLevel, UnaryOperator, FUNCTION_PARAM_REGISTERS,
};

struct StackContext<'a> {
    current_min_pointer: i32,
    stack_locations: HashMap<String, i32>,
    symbols: &'a mut HashMap<String, SymbolInfo>,
}

impl Convert for Program {
    type Input = BirdsProgramNode;
    type Output = Self;

    fn convert(
        parsed: BirdsProgramNode,
        context: &mut ConvertContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(Program {
            body: Vec::<TopLevel>::convert(parsed.body, context)?,
            displaying_context: DisplayContext {
                comments: context.comments,
                indent: 0,
                word_length_bytes: 4,
                is_linux: context.is_linux,
                is_mac: context.is_mac,
                symbols: context.symbols.clone(),
            },
        })
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
            BirdsTopLevel::StaticVariable(name, init, global) => {
                Ok(TopLevel::StaticVariable(name, init, global))
            }
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
        let mut instructions: Vec<Instruction> = Vec::new();

        // ZEROTH PASS: Copy all the params out of registers onto the stack.
        for (i, param) in params.iter().enumerate() {
            if i < 6 {
                instructions.push(Instruction::Mov(
                    Operand::Reg(FUNCTION_PARAM_REGISTERS[i].clone()),
                    Operand::MockReg(param.to_string()),
                ));
            } else {
                let unsigned_i: i32 = i.try_into().unwrap();
                instructions.push(Instruction::Mov(
                    Operand::Stack(8 * (unsigned_i - 4)), // +24 as in, this is copying from the CALLER's frame
                    Operand::MockReg(param.to_string()),
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

        // SECOND PASS: replace every mock register with an entry on the stack, making sure to keep
        // a map from register names to stack locations
        let mut stack_context = StackContext {
            current_min_pointer: 0,
            stack_locations: HashMap::new(),
            symbols: &mut context.symbols,
        };

        let instructions = TopLevel::update_instructions_with_context(
            instructions,
            Instruction::replace_mock_registers,
            &mut stack_context,
        );

        // THIRD PASS: use the value of current_max_pointer to add an instruction to the start of
        // the function
        let instructions: Vec<Instruction> = vec![
            // push the address of RBP to the stack (at RSP).
            Instruction::Custom("pushq %rbp".to_string()),
            // move RBP to RSP's current location.
            Instruction::Custom("movq %rsp, %rbp".to_string()),
            // allocate space for local variables in the space before RSP in the stack.
            Instruction::AllocateStack(
                (-stack_context.current_min_pointer)
                    + (stack_context.current_min_pointer).rem_euclid(16),
            ),
        ]
        .into_iter()
        .chain(instructions)
        .collect();

        let instructions =
            TopLevel::update_instructions(instructions, Instruction::fix_shift_operation_register);
        let instructions = TopLevel::update_instructions(
            instructions,
            Instruction::fix_instructions_with_two_stack_entries,
        );
        let instructions = TopLevel::update_instructions(
            instructions,
            Instruction::fix_immediate_values_in_bad_places,
        );
        let instructions =
            TopLevel::update_instructions(instructions, Instruction::fix_memory_address_as_dst);

        let instructions =
            TopLevel::update_instructions(instructions, Instruction::fix_constant_as_dst);

        Ok(TopLevel::Function(name, instructions, global))
    }
}

impl TopLevel {
    fn update_instructions(
        instructions: Vec<Instruction>,
        f: fn(Instruction) -> Vec<Instruction>,
    ) -> Vec<Instruction> {
        instructions.into_iter().flat_map(f).collect()
    }

    fn update_instructions_with_context<C>(
        instructions: Vec<Instruction>,
        f: fn(Instruction, &mut C) -> Vec<Instruction>,
        context: &mut C,
    ) -> Vec<Instruction> {
        instructions
            .into_iter()
            .flat_map(|instruction| f(instruction, context))
            .collect()
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
            BirdsInstructionNode::Return(src) => vec![
                Instruction::Mov(Operand::convert(src, context)?, Operand::Reg(Register::AX)),
                Instruction::Ret,
            ],
            BirdsInstructionNode::Unary(BirdsUnaryOperatorNode::Not, src, dst) => vec![
                // Not returns 1 is src is zero, and 0 otherwise.
                Instruction::Cmp(Operand::Imm(0), Operand::convert(src, context)?),
                // zero out the destination (since SetCondition only affects the first byte).
                Instruction::Mov(Operand::Imm(0), Operand::convert(dst.clone(), context)?),
                Instruction::SetCondition(ConditionCode::E, Operand::convert(dst, context)?),
            ],
            BirdsInstructionNode::Unary(op, src, dst) => vec![
                Instruction::Mov(
                    Operand::convert(src, context)?,
                    Operand::convert(dst.clone(), context)?,
                ),
                Instruction::Unary(
                    UnaryOperator::convert(op, context)?,
                    Operand::convert(dst, context)?,
                ),
            ],
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Divide, left, right, dst) => {
                vec![
                    Instruction::Mov(Operand::convert(left, context)?, Operand::Reg(Register::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(Operand::convert(right, context)?),
                    Instruction::Mov(
                        Operand::Reg(Register::AX), // read quotient result from EAX
                        Operand::convert(dst, context)?,
                    ),
                ]
            }
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Mod, left, right, dst) => vec![
                Instruction::Mov(Operand::convert(left, context)?, Operand::Reg(Register::AX)),
                Instruction::Cdq,
                // note: idiv can't operate on immediate values, so we need to stuff those into
                // a register during a later pass.
                Instruction::Idiv(Operand::convert(right, context)?),
                Instruction::Mov(
                    Operand::Reg(Register::DX), // read remainder result from EDX
                    Operand::convert(dst, context)?,
                ),
            ],
            BirdsInstructionNode::Binary(op, left, right, dst) if op.is_relational() => vec![
                Instruction::Cmp(
                    Operand::convert(right, context)?,
                    Operand::convert(left, context)?,
                ),
                // zero out the destination (since SetCondition only affects the first byte).
                Instruction::Mov(Operand::Imm(0), Operand::convert(dst.clone(), context)?),
                Instruction::SetCondition(
                    ConditionCode::convert(op, context)?,
                    Operand::convert(dst, context)?,
                ),
            ],
            BirdsInstructionNode::Binary(op, left, right, dst) => vec![
                Instruction::Mov(
                    Operand::convert(left, context)?,
                    Operand::convert(dst.clone(), context)?,
                ),
                Instruction::Binary(
                    BinaryOperator::convert(op, context)?,
                    Operand::convert(right, context)?,
                    Operand::convert(dst, context)?,
                ),
            ],
            BirdsInstructionNode::Copy(src, dst) => {
                vec![Instruction::Mov(
                    Operand::convert(src, context)?,
                    Operand::convert(dst, context)?,
                )]
            }
            BirdsInstructionNode::Jump(s) => vec![Instruction::Jmp(s)],
            BirdsInstructionNode::JumpZero(src, s) => vec![
                Instruction::Cmp(Operand::Imm(0), Operand::convert(src, context)?),
                Instruction::JmpCondition(ConditionCode::E, s),
            ],
            BirdsInstructionNode::JumpNotZero(src, s) => vec![
                Instruction::Cmp(Operand::Imm(0), Operand::convert(src, context)?),
                Instruction::JmpCondition(ConditionCode::Ne, s),
            ],
            BirdsInstructionNode::Label(s) => vec![Instruction::Label(s)],
            BirdsInstructionNode::FunctionCall(name, args, dst) => {
                let mut instructions = Vec::new();
                let (register_args, stack_args) = split(args, 6);
                let stack_padding: usize = if stack_args.len() % 2 == 1 { 8 } else { 0 };

                if stack_padding != 0 {
                    instructions.push(Instruction::AllocateStack(
                        stack_padding.try_into().unwrap(),
                    ));
                }

                for (i, arg) in register_args.iter().enumerate() {
                    instructions.push(Instruction::Mov(
                        Operand::convert(arg.clone(), context)?,
                        Operand::Reg(FUNCTION_PARAM_REGISTERS[i].clone()),
                    ));
                }

                for arg in stack_args.iter().rev() {
                    let converted_arg = Operand::convert(arg.clone(), context)?;
                    if !matches!(converted_arg, Operand::Reg(_) | Operand::Imm(_)) {
                        instructions.push(Instruction::Mov(
                            converted_arg.clone(),
                            Operand::Reg(Register::AX),
                        ));
                        instructions.push(Instruction::Push(Operand::Reg(Register::AX)));
                    } else {
                        instructions.push(Instruction::Push(converted_arg));
                    }
                }

                instructions.push(Instruction::Call(name));

                let callee_stack_size = 8 * stack_args.len() + stack_padding;
                if callee_stack_size != 0 {
                    instructions.push(Instruction::FreeStack(
                        callee_stack_size.try_into().unwrap(),
                    ));
                }
                instructions.push(Instruction::Mov(
                    Operand::Reg(Register::AX),
                    Operand::convert(dst.clone(), context)?,
                ));

                instructions
            }
        })
    }
}

fn split<T>(vec: Vec<T>, index: usize) -> (Vec<T>, Vec<T>) {
    let mut right = vec;
    let left = right.drain(..min(index, right.len())).collect();
    (left, right)
}

impl Instruction {
    fn replace_mock_registers(mut self, context: &mut StackContext) -> Vec<Instruction> {
        match self {
            Instruction::Mov(ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Unary(_, ref mut dst) => {
                dst.replace_mock_register(context);
            }
            Instruction::Binary(_, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Idiv(ref mut src) => {
                src.replace_mock_register(context);
            }
            Instruction::Cmp(ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Cdq => {}
            Instruction::Jmp(_) => {}
            Instruction::JmpCondition(_, _) => {}
            Instruction::SetCondition(_, ref mut dst) => {
                dst.replace_mock_register(context);
            }
            Instruction::Label(_) => {}
            Instruction::AllocateStack(_) => {}
            Instruction::Ret => {}
            Instruction::Custom(_) => {}
            Instruction::FreeStack(_) => {}
            Instruction::Push(ref mut src) => {
                src.replace_mock_register(context);
            }
            Instruction::Call(_) => {}
        };
        vec![self]
    }

    fn fix_instructions_with_two_stack_entries(self) -> Vec<Instruction> {
        let (src, dst) = match self {
            Instruction::Mov(ref src, ref dst) => (src, dst),
            Instruction::Binary(_, ref src, ref dst) => (src, dst),
            // Instruction::Mov(Operand::Stack(src), Operand::Reg(Register::R10)),
            // Instruction::Binary(op, Operand::Reg(Register::R10), Operand::Stack(dst)),
            Instruction::Cmp(ref left, ref right) => (left, right),
            _ => return vec![self],
        };
        if matches!(src, &Operand::Stack(_) | &Operand::Data(_))
            && matches!(dst, &Operand::Stack(_) | &Operand::Data(_))
        {
            match self {
                Instruction::Mov(src, dst) => vec![
                    Instruction::Mov(src, Operand::Reg(Register::R10)),
                    Instruction::Mov(Operand::Reg(Register::R10), dst),
                ],
                Instruction::Binary(op, src, dst) => vec![
                    Instruction::Mov(src, Operand::Reg(Register::R10)),
                    Instruction::Binary(op, Operand::Reg(Register::R10), dst),
                ],
                Instruction::Cmp(left, right) => vec![
                    Instruction::Mov(left, Operand::Reg(Register::R10)),
                    Instruction::Cmp(Operand::Reg(Register::R10), right),
                ],
                _ => unreachable!(),
            }
        } else {
            vec![self]
        }
    }

    fn fix_immediate_values_in_bad_places(self) -> Vec<Instruction> {
        match self {
            Instruction::Idiv(Operand::Imm(value)) => vec![
                Instruction::Mov(
                    Operand::Imm(value),
                    // we use r10d to fix src's
                    Operand::Reg(Register::R10),
                ),
                Instruction::Idiv(Operand::Reg(Register::R10)),
            ],
            _ => vec![self],
        }
    }

    fn fix_memory_address_as_dst(self) -> Vec<Instruction> {
        match self {
            Instruction::Binary(BinaryOperator::Mult, src, Operand::Stack(value)) => vec![
                Instruction::Mov(Operand::Stack(value), Operand::Reg(Register::R11)),
                Instruction::Binary(
                    BinaryOperator::Mult,
                    src.clone(),
                    Operand::Reg(Register::R11), // we use r11d to fix dst's
                ),
                Instruction::Mov(Operand::Reg(Register::R11), Operand::Stack(value)),
            ],
            _ => vec![self],
        }
    }

    fn fix_shift_operation_register(self) -> Vec<Instruction> {
        match self {
            Instruction::Binary(BinaryOperator::ShiftLeft, left, right) => {
                vec![
                    Instruction::Mov(left, Operand::Reg(Register::CX)),
                    Instruction::Binary(
                        BinaryOperator::ShiftLeft,
                        Operand::Reg(Register::CX),
                        right,
                    ),
                ]
            }
            Instruction::Binary(BinaryOperator::ShiftRight, left, right) => {
                vec![
                    Instruction::Mov(left, Operand::Reg(Register::CX)),
                    Instruction::Binary(
                        BinaryOperator::ShiftRight,
                        Operand::Reg(Register::CX),
                        right,
                    ),
                ]
            }
            _ => vec![self],
        }
    }

    fn fix_constant_as_dst(self) -> Vec<Instruction> {
        match self {
            Instruction::Cmp(left, Operand::Imm(value)) => vec![
                Instruction::Mov(Operand::Imm(value), Operand::Reg(Register::R11)),
                Instruction::Cmp(left, Operand::Reg(Register::R11)),
            ],
            _ => vec![self],
        }
    }
}

impl Convert for Operand {
    type Input = BirdsValueNode;
    type Output = Self;

    fn convert(
        input: BirdsValueNode,
        _context: &mut ConvertContext,
    ) -> Result<Operand, Box<dyn Error>> {
        match input {
            BirdsValueNode::IntegerConstant(c) => Ok(Operand::Imm(c)),
            BirdsValueNode::Var(s) => Ok(Operand::MockReg(s)),
        }
    }
}

impl Operand {
    fn replace_mock_register(&mut self, context: &mut StackContext) {
        if let Operand::MockReg(name) = self {
            let existing_location = context.stack_locations.get(name);
            if let Some(loc) = existing_location {
                *self = Operand::Stack(*loc);
                return;
            }
            match context.symbols.get(name).map(|res| &res.storage) {
                Some(StorageInfo::Static(_init, _global)) => {
                    *self = Operand::Data(name.clone());
                }
                None | Some(StorageInfo::Automatic) | Some(StorageInfo::Function(_, _)) => {
                    context.current_min_pointer -= 4;
                    let new_location = context.current_min_pointer;
                    context.stack_locations.insert(name.clone(), new_location);
                    *self = Operand::Stack(new_location);
                }
            }
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
