use crate::compiler::{
    birds::{
        BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsInstructions, BirdsProgramNode,
        BirdsUnaryOperatorNode, BirdsValueNode,
    },
    lexer::Type,
};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

use super::{
    BinaryOperator, Convert, ConvertContext, ExtraStrings, Function, Instruction, Instructions,
    Operand, Program, Register, UnaryOperator,
};

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
                is_mac: config.is_mac,
            },
            footer: if config.is_linux {
                // linux security thing: tell the program we do not need the stack to be
                // executable.
                if config.comments {
                    ExtraStrings(vec![
                        "#tell the program we don't need an executable stack".to_string(),
                        ".section .note.GNU-stack,\"\",@progbits".to_string(),
                    ])
                } else {
                    ExtraStrings(vec![".section .note.GNU-stack,\"\",@progbits".to_string()])
                }
            } else {
                ExtraStrings(Vec::new())
            },
            has_comments: config.comments,
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
        let mut instructions: VecDeque<Instruction> = VecDeque::new();
        for instruction in input.0.into_iter() {
            let mut converted_instructions = Instruction::convert(instruction, config)?;
            instructions.append(&mut converted_instructions);
        }

        // SECOND PASS: replace every mock register with an entry on the stack, making sure to keep
        // a map from register names to stack locations
        let mut stack_locations: HashMap<String, i32> = HashMap::new();
        let mut current_max_pointer = 0;
        for instruction in instructions.iter_mut() {
            instruction.replace_mock_registers(&mut stack_locations, &mut current_max_pointer);
        }

        // THIRD PASS: use the value of current_max_pointer to add an instruction to the start of
        // the function
        let instructions: VecDeque<Instruction> = vec![
            // push the address of RBP to the stack (at RSP).
            Instruction::Custom("pushq %rbp".to_string()),
            // move RBP to RSP's current location.
            Instruction::Custom("movq %rsp, %rbp".to_string()),
            // allocate space for local variables in the space before RSP in the stack.
            Instruction::AllocateStack(-current_max_pointer),
        ]
        .into_iter()
        .chain(instructions)
        .collect();

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

        Ok(Instructions(instructions))
    }
}

impl Instructions {
    fn update_instructions(
        instructions: VecDeque<Instruction>,
        f: fn(&Instruction) -> Option<VecDeque<Instruction>>,
    ) -> VecDeque<Instruction> {
        let mut new_instructions: VecDeque<Instruction> = VecDeque::new();
        for instruction in instructions.into_iter() {
            if let Some(mut res) = f(&instruction) {
                new_instructions.append(&mut res);
            } else {
                new_instructions.push_back(instruction);
            }
        }
        new_instructions
    }
}

impl Convert for Instruction {
    type Input = BirdsInstructionNode;
    type Output = VecDeque<Instruction>;

    fn convert(
        input: BirdsInstructionNode,
        config: &mut ConvertContext,
    ) -> Result<VecDeque<Instruction>, Box<dyn Error>> {
        match input {
            BirdsInstructionNode::Return(src) => Ok(vec![
                Instruction::Mov(Operand::convert(src, config)?, Operand::Reg(Register::AX)),
                Instruction::Ret,
            ]
            .into()),
            BirdsInstructionNode::Unary(op, src, dst) => Ok(vec![
                Instruction::Mov(
                    Operand::convert(src, config)?,
                    Operand::convert(dst.clone(), config)?,
                ),
                Instruction::Unary(
                    UnaryOperator::convert(op, config)?,
                    Operand::convert(dst, config)?,
                ),
            ]
            .into()),
            BirdsInstructionNode::Binary(op, left, right, dst) => match op {
                BirdsBinaryOperatorNode::Divide => Ok(vec![
                    Instruction::Mov(Operand::convert(left, config)?, Operand::Reg(Register::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(Operand::convert(right, config)?),
                    Instruction::Mov(
                        Operand::Reg(Register::AX), // read quotient result from EAX
                        Operand::convert(dst, config)?,
                    ),
                ]
                .into()),
                BirdsBinaryOperatorNode::Mod => Ok(vec![
                    Instruction::Mov(Operand::convert(left, config)?, Operand::Reg(Register::AX)),
                    Instruction::Cdq,
                    // note: idiv can't operate on immediate values, so we need to stuff those into
                    // a register during a later pass.
                    Instruction::Idiv(Operand::convert(right, config)?),
                    Instruction::Mov(
                        Operand::Reg(Register::DX), // read remainder result from EDX
                        Operand::convert(dst, config)?,
                    ),
                ]
                .into()),
                _ => Ok(vec![
                    Instruction::Mov(
                        Operand::convert(left, config)?,
                        Operand::convert(dst.clone(), config)?,
                    ),
                    Instruction::Binary(
                        BinaryOperator::convert(op, config)?,
                        Operand::convert(right, config)?,
                        Operand::convert(dst, config)?,
                    ),
                ]
                .into()),
            },
            BirdsInstructionNode::Copy(_, _) => todo!(),
            BirdsInstructionNode::Jump(_) => todo!(),
            BirdsInstructionNode::JumpZero(_, _) => todo!(),
            BirdsInstructionNode::JumpNotZero(_, _) => todo!(),
            BirdsInstructionNode::Label(_) => todo!(),
        }
    }
}

impl Instruction {
    fn replace_mock_registers(
        &mut self,
        stack_locations: &mut HashMap<String, i32>,
        current_max_pointer: &mut i32,
    ) {
        match self {
            Instruction::Mov(src, dst) => {
                src.replace_mock_register(stack_locations, current_max_pointer);
                dst.replace_mock_register(stack_locations, current_max_pointer);
            }
            Instruction::Unary(_, dst) => {
                dst.replace_mock_register(stack_locations, current_max_pointer);
            }
            Instruction::Binary(_, src, dst) => {
                src.replace_mock_register(stack_locations, current_max_pointer);
                dst.replace_mock_register(stack_locations, current_max_pointer);
            }
            Instruction::Idiv(src) => {
                src.replace_mock_register(stack_locations, current_max_pointer);
            }
            _ => {}
        }
    }

    fn fix_instructions_with_two_stack_entries(&self) -> Option<VecDeque<Instruction>> {
        match self {
            Instruction::Mov(Operand::Stack(src), Operand::Stack(dst)) => Some(
                vec![
                    Instruction::Mov(Operand::Stack(*src), Operand::Reg(Register::R10)),
                    Instruction::Mov(Operand::Reg(Register::R10), Operand::Stack(*dst)),
                ]
                .into(),
            ),
            Instruction::Binary(op, Operand::Stack(src), Operand::Stack(dst)) => Some(
                vec![
                    Instruction::Mov(Operand::Stack(*src), Operand::Reg(Register::R10)),
                    Instruction::Binary(
                        op.clone(),
                        Operand::Reg(Register::R10),
                        Operand::Stack(*dst),
                    ),
                ]
                .into(),
            ),
            _ => None,
        }
    }

    fn fix_immediate_values_in_bad_places(&self) -> Option<VecDeque<Instruction>> {
        match self {
            Instruction::Idiv(Operand::Imm(value)) => Some(
                vec![
                    Instruction::Mov(
                        Operand::Imm(*value),
                        // we use r10d to fix src's
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Idiv(Operand::Reg(Register::R10)),
                ]
                .into(),
            ),
            _ => None,
        }
    }

    fn fix_memory_address_as_dst(&self) -> Option<VecDeque<Instruction>> {
        match self {
            Instruction::Binary(BinaryOperator::Mult, src, Operand::Stack(value)) => Some(
                vec![
                    Instruction::Mov(Operand::Stack(*value), Operand::Reg(Register::R11)),
                    Instruction::Binary(
                        BinaryOperator::Mult,
                        src.clone(),
                        Operand::Reg(Register::R11), // we use r11d to fix dst's
                    ),
                    Instruction::Mov(Operand::Reg(Register::R11), Operand::Stack(*value)),
                ]
                .into(),
            ),
            _ => None,
        }
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
            BirdsValueNode::Constant(Type::Integer(c)) => Ok(Operand::Imm(c)),
            BirdsValueNode::Var(s) => Ok(Operand::MockReg(s)),
        }
    }
}
impl Operand {
    fn replace_mock_register(
        &mut self,
        stack_locations: &mut HashMap<String, i32>,
        current_max_pointer: &mut i32,
    ) {
        if let Operand::MockReg(name) = self {
            let new_location = stack_locations.entry(name.to_string()).or_insert_with(|| {
                *current_max_pointer -= 4;
                *current_max_pointer
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
            BirdsUnaryOperatorNode::Not => todo!(),
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
            BirdsBinaryOperatorNode::Equal => todo!(),
            BirdsBinaryOperatorNode::NotEqual => todo!(),
            BirdsBinaryOperatorNode::Less => todo!(),
            BirdsBinaryOperatorNode::Greater => todo!(),
            BirdsBinaryOperatorNode::LessEqual => todo!(),
            BirdsBinaryOperatorNode::GreaterEqual => todo!(),
            BirdsBinaryOperatorNode::Divide | BirdsBinaryOperatorNode::Mod => {
                panic!("should not treat mod and divide as binary expressions during codegen")
            }
        }
    }
}
