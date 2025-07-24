use crate::compiler::birds::{
    BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsInstructions, BirdsProgramNode,
    BirdsUnaryOperatorNode, BirdsValueNode,
};
use itertools::process_results;
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

use super::{
    BinaryOperator, ConditionCode, Convert, ConvertContext, DisplayContext, ExtraStrings, Function,
    Instruction, Instructions, Operand, Program, Register, UnaryOperator,
};

struct StackContext {
    current_min_pointer: i32,
    stack_locations: HashMap<String, i32>,
}

impl Convert for Program {
    type Input = BirdsProgramNode;
    type Output = Self;

    fn convert(
        parsed: BirdsProgramNode,
        config: &mut ConvertContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(Program {
            function: Function {
                header: if config.comments {
                    ExtraStrings(vec![
                        "# make the function name accessible globally".to_string(),
                        format!(".globl {}", parsed.function.name),
                    ])
                } else {
                    ExtraStrings(vec![format!(".globl {}", parsed.function.name)])
                },
                name: parsed.function.name,
                instructions: Instructions::convert(parsed.function.instructions, config)?,
            },
            footer: if config.is_linux {
                // linux security thing: tell the program we do not need the stack to be
                // executable.
                if config.comments {
                    ExtraStrings(vec![
                        "# Tell the program we don't need an executable stack".to_string(),
                        ".section .note.GNU-stack,\"\",@progbits".to_string(),
                    ])
                } else {
                    ExtraStrings(vec![".section .note.GNU-stack,\"\",@progbits".to_string()])
                }
            } else {
                ExtraStrings(Vec::new())
            },
            displaying_context: DisplayContext {
                comments: config.comments,
                indent: 0,
                word_length_bytes: 4,
                is_mac: config.is_mac,
            },
        })
    }
}

impl Convert for Instructions {
    type Input = BirdsInstructions;
    type Output = Self;
    fn convert(
        input: BirdsInstructions,
        config: &mut ConvertContext,
    ) -> Result<Instructions, Box<dyn Error>> {
        // FIRST PASS: create a bunch of mock registers to be replaced with stack entries later
        let instructions: VecDeque<Instruction> = process_results(
            input
                .0
                .into_iter()
                .map(|instruction| Instruction::convert(instruction, config)),
            |iter| iter.flatten().collect(),
        )?;

        // SECOND PASS: replace every mock register with an entry on the stack, making sure to keep
        // a map from register names to stack locations
        let mut stack_context = StackContext {
            current_min_pointer: 0,
            stack_locations: HashMap::new(),
        };

        let instructions = Instructions::update_instructions_with_context(
            instructions,
            Instruction::replace_mock_registers,
            &mut stack_context,
        );

        // THIRD PASS: use the value of current_max_pointer to add an instruction to the start of
        // the function
        let instructions: VecDeque<Instruction> = vec![
            // push the address of RBP to the stack (at RSP).
            Instruction::Custom("pushq %rbp".to_string()),
            // move RBP to RSP's current location.
            Instruction::Custom("movq %rsp, %rbp".to_string()),
            // allocate space for local variables in the space before RSP in the stack.
            Instruction::AllocateStack(-stack_context.current_min_pointer),
        ]
        .into_iter()
        .chain(instructions)
        .collect();

        let instructions = Instructions::update_instructions(
            instructions,
            Instruction::fix_shift_operation_register,
        );
        let instructions = Instructions::update_instructions(
            instructions,
            Instruction::fix_instructions_with_two_stack_entries,
        );
        let instructions = Instructions::update_instructions(
            instructions,
            Instruction::fix_immediate_values_in_bad_places,
        );
        let instructions =
            Instructions::update_instructions(instructions, Instruction::fix_memory_address_as_dst);

        let instructions =
            Instructions::update_instructions(instructions, Instruction::fix_constant_as_dst);

        Ok(Instructions(instructions))
    }
}

impl Instructions {
    fn update_instructions(
        instructions: VecDeque<Instruction>,
        f: fn(Instruction) -> VecDeque<Instruction>,
    ) -> VecDeque<Instruction> {
        instructions.into_iter().flat_map(f).collect()
    }

    fn update_instructions_with_context<C>(
        instructions: VecDeque<Instruction>,
        f: fn(Instruction, &mut C) -> VecDeque<Instruction>,
        context: &mut C,
    ) -> VecDeque<Instruction> {
        instructions
            .into_iter()
            .flat_map(|instruction| f(instruction, context))
            .collect()
    }
}

impl Convert for Instruction {
    type Input = BirdsInstructionNode;
    type Output = VecDeque<Instruction>;

    fn convert(
        input: BirdsInstructionNode,
        config: &mut ConvertContext,
    ) -> Result<VecDeque<Instruction>, Box<dyn Error>> {
        Ok(match input {
            BirdsInstructionNode::Return(src) => vec![
                Instruction::Mov(Operand::convert(src, config)?, Operand::Reg(Register::AX)),
                Instruction::Ret,
            ],
            BirdsInstructionNode::Unary(BirdsUnaryOperatorNode::Not, src, dst) => vec![
                // Not returns 1 is src is zero, and 0 otherwise.
                Instruction::Cmp(Operand::Imm(0), Operand::convert(src, config)?),
                // zero out the destination (since SetCondition only affects the first byte).
                Instruction::Mov(Operand::Imm(0), Operand::convert(dst.clone(), config)?),
                Instruction::SetCondition(ConditionCode::E, Operand::convert(dst, config)?),
            ],
            BirdsInstructionNode::Unary(op, src, dst) => vec![
                Instruction::Mov(
                    Operand::convert(src, config)?,
                    Operand::convert(dst.clone(), config)?,
                ),
                Instruction::Unary(
                    UnaryOperator::convert(op, config)?,
                    Operand::convert(dst, config)?,
                ),
            ],
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Divide, left, right, dst) => {
                vec![
                    Instruction::Mov(Operand::convert(left, config)?, Operand::Reg(Register::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(Operand::convert(right, config)?),
                    Instruction::Mov(
                        Operand::Reg(Register::AX), // read quotient result from EAX
                        Operand::convert(dst, config)?,
                    ),
                ]
            }
            BirdsInstructionNode::Binary(BirdsBinaryOperatorNode::Mod, left, right, dst) => vec![
                Instruction::Mov(Operand::convert(left, config)?, Operand::Reg(Register::AX)),
                Instruction::Cdq,
                // note: idiv can't operate on immediate values, so we need to stuff those into
                // a register during a later pass.
                Instruction::Idiv(Operand::convert(right, config)?),
                Instruction::Mov(
                    Operand::Reg(Register::DX), // read remainder result from EDX
                    Operand::convert(dst, config)?,
                ),
            ],
            BirdsInstructionNode::Binary(op, left, right, dst) if op.is_relational() => vec![
                Instruction::Cmp(
                    Operand::convert(right, config)?,
                    Operand::convert(left, config)?,
                ),
                // zero out the destination (since SetCondition only affects the first byte).
                Instruction::Mov(Operand::Imm(0), Operand::convert(dst.clone(), config)?),
                Instruction::SetCondition(
                    ConditionCode::convert(op, config)?,
                    Operand::convert(dst, config)?,
                ),
            ],
            BirdsInstructionNode::Binary(op, left, right, dst) => vec![
                Instruction::Mov(
                    Operand::convert(left, config)?,
                    Operand::convert(dst.clone(), config)?,
                ),
                Instruction::Binary(
                    BinaryOperator::convert(op, config)?,
                    Operand::convert(right, config)?,
                    Operand::convert(dst, config)?,
                ),
            ],
            BirdsInstructionNode::Copy(src, dst) => {
                vec![Instruction::Mov(
                    Operand::convert(src, config)?,
                    Operand::convert(dst, config)?,
                )]
            }
            BirdsInstructionNode::Jump(s) => vec![Instruction::Jmp(s)],
            BirdsInstructionNode::JumpZero(src, s) => vec![
                Instruction::Cmp(Operand::Imm(0), Operand::convert(src, config)?),
                Instruction::JmpCondition(ConditionCode::E, s),
            ],
            BirdsInstructionNode::JumpNotZero(src, s) => vec![
                Instruction::Cmp(Operand::Imm(0), Operand::convert(src, config)?),
                Instruction::JmpCondition(ConditionCode::Ne, s),
            ],
            BirdsInstructionNode::Label(s) => vec![Instruction::Label(s)],
        }
        .into())
    }
}

impl Instruction {
    fn replace_mock_registers(mut self, context: &mut StackContext) -> VecDeque<Instruction> {
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
        };
        vec![self].into()
    }

    fn fix_instructions_with_two_stack_entries(self) -> VecDeque<Instruction> {
        match self {
            Instruction::Mov(Operand::Stack(src), Operand::Stack(dst)) => vec![
                Instruction::Mov(Operand::Stack(src), Operand::Reg(Register::R10)),
                Instruction::Mov(Operand::Reg(Register::R10), Operand::Stack(dst)),
            ],
            Instruction::Binary(op, Operand::Stack(src), Operand::Stack(dst)) => vec![
                Instruction::Mov(Operand::Stack(src), Operand::Reg(Register::R10)),
                Instruction::Binary(op, Operand::Reg(Register::R10), Operand::Stack(dst)),
            ],
            Instruction::Cmp(Operand::Stack(left), Operand::Stack(right)) => vec![
                Instruction::Mov(Operand::Stack(left), Operand::Reg(Register::R10)),
                Instruction::Cmp(Operand::Reg(Register::R10), Operand::Stack(right)),
            ],
            _ => vec![self],
        }
        .into()
    }

    fn fix_immediate_values_in_bad_places(self) -> VecDeque<Instruction> {
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
        .into()
    }

    fn fix_memory_address_as_dst(self) -> VecDeque<Instruction> {
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
        .into()
    }

    fn fix_shift_operation_register(self) -> VecDeque<Instruction> {
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
        .into()
    }

    fn fix_constant_as_dst(self) -> VecDeque<Instruction> {
        match self {
            Instruction::Cmp(left, Operand::Imm(value)) => vec![
                Instruction::Mov(Operand::Imm(value), Operand::Reg(Register::R11)),
                Instruction::Cmp(left, Operand::Reg(Register::R11)),
            ],
            _ => vec![self],
        }
        .into()
    }
}

impl Convert for Operand {
    type Input = BirdsValueNode;
    type Output = Self;

    fn convert(
        input: BirdsValueNode,
        _config: &mut ConvertContext,
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
            let new_location = context
                .stack_locations
                .entry(name.to_string())
                .or_insert_with(|| {
                    context.current_min_pointer -= 4;
                    context.current_min_pointer
                });
            *self = Operand::Stack(*new_location);
        }
    }
}

impl Convert for UnaryOperator {
    type Input = BirdsUnaryOperatorNode;
    type Output = Self;

    fn convert(
        input: BirdsUnaryOperatorNode,
        _config: &mut ConvertContext,
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
        _config: &mut ConvertContext,
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
        _config: &mut ConvertContext,
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
