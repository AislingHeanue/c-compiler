use super::{
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

mod display;

pub struct AssemblyProgramNode {
    function: AssemblyFunctionNode,
    footer: ExtraStrings,
    has_comments: bool,
}

struct ExtraStrings(Vec<String>);

struct AssemblyFunctionNode {
    header: ExtraStrings,
    name: String,
    instructions: Instructions,
    is_mac: bool,
}

struct Instructions(VecDeque<InstructionNode>);

#[derive(Clone)]
enum InstructionNode {
    Mov(OperandNode, OperandNode),
    Unary(AssemblyUnaryOperatorNode, OperandNode), // Operand here is both the src and dst.
    // op src, dst. dst is the *first* number in the operation
    Binary(AssemblyBinaryOperatorNode, OperandNode, OperandNode),
    // dividend comes from EDX+EAX. quotient -> EDX, remainder -> EAX.
    Idiv(OperandNode),
    // expand a 32 bit number to 64 bits. EAX -> EDX+EAX.
    Cdq,
    AllocateStack(i32), // number of bytes to allocate
    Custom(String),
    Ret,
}

#[derive(Clone)]
enum OperandNode {
    Imm(i32),        //constant numeric value
    Reg(Register),   // register in assembly
    MockReg(String), // mocked register for temporary use.
    Stack(i32),      // Stack entry whose value is the offset from RSP.
}

#[derive(Clone)]
enum Register {
    AX, // eax or rax
    DX, // edx or rdx
    R10,
    R11,
}

#[derive(Clone)]
enum AssemblyUnaryOperatorNode {
    Neg,
    Not,
}

#[derive(Clone)]
enum AssemblyBinaryOperatorNode {
    Add,
    Sub,
    Mult,
}

pub fn codegen(
    parsed: BirdsProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
) -> Result<AssemblyProgramNode, Box<dyn Error>> {
    AssemblyProgramNode::convert(parsed, comments, linux, mac)
}

impl AssemblyProgramNode {
    fn convert(
        parsed: BirdsProgramNode,
        comments: bool,
        linux: bool,
        mac: bool,
    ) -> Result<AssemblyProgramNode, Box<dyn Error>> {
        Ok(AssemblyProgramNode {
            function: AssemblyFunctionNode {
                header: if comments {
                    ExtraStrings(vec![
                        "# make the function name accessible globally".to_string(),
                        format!(".globl {}", parsed.function.name),
                    ])
                } else {
                    ExtraStrings(vec![format!(".globl {}", parsed.function.name)])
                },
                name: parsed.function.name,
                instructions: Instructions::convert(parsed.function.instructions)?,
                is_mac: mac,
            },
            footer: if linux {
                // linux security thing: tell the program we do not need the stack to be
                // executable.
                if comments {
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
            has_comments: comments,
        })
    }
}

impl Instructions {
    fn convert(input: BirdsInstructions) -> Result<Instructions, Box<dyn Error>> {
        // FIRST PASS: create a bunch of mock registers to be replaced with stack entries later
        let mut instructions: VecDeque<InstructionNode> = VecDeque::new();
        for instruction in input.0.into_iter() {
            let mut converted_instructions = InstructionNode::convert(instruction)?;
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
        let instructions: VecDeque<InstructionNode> = vec![
            // push the address of RBP to the stack (at RSP).
            InstructionNode::Custom("pushq %rbp".to_string()),
            // move RBP to RSP's current location.
            InstructionNode::Custom("movq %rsp, %rbp".to_string()),
            // allocate space for local variables in the space before RSP in the stack.
            InstructionNode::AllocateStack(-current_max_pointer),
        ]
        .into_iter()
        .chain(instructions)
        .collect();

        let instructions = Instructions::update_instructions(
            instructions,
            InstructionNode::fix_instructions_with_two_stack_entries,
        );
        let instructions = Instructions::update_instructions(
            instructions,
            InstructionNode::fix_immediate_values_in_bad_places,
        );
        let instructions = Instructions::update_instructions(
            instructions,
            InstructionNode::fix_memory_address_as_dst,
        );

        Ok(Instructions(instructions))
    }

    fn update_instructions(
        instructions: VecDeque<InstructionNode>,
        f: fn(&InstructionNode) -> Option<VecDeque<InstructionNode>>,
    ) -> VecDeque<InstructionNode> {
        let mut new_instructions: VecDeque<InstructionNode> = VecDeque::new();
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

impl InstructionNode {
    fn convert(input: BirdsInstructionNode) -> Result<VecDeque<InstructionNode>, Box<dyn Error>> {
        match input {
            BirdsInstructionNode::Return(src) => Ok(vec![
                InstructionNode::Mov(OperandNode::convert(src)?, OperandNode::Reg(Register::AX)),
                InstructionNode::Ret,
            ]
            .into()),
            BirdsInstructionNode::Unary(op, src, dst) => Ok(vec![
                InstructionNode::Mov(
                    OperandNode::convert(src)?,
                    OperandNode::convert(dst.clone())?,
                ),
                InstructionNode::Unary(
                    AssemblyUnaryOperatorNode::convert(op)?,
                    OperandNode::convert(dst)?,
                ),
            ]
            .into()),
            BirdsInstructionNode::Binary(op, left, right, dst) => match op {
                BirdsBinaryOperatorNode::Divide => Ok(vec![
                    InstructionNode::Mov(
                        OperandNode::convert(left)?,
                        OperandNode::Reg(Register::AX),
                    ),
                    InstructionNode::Cdq,
                    InstructionNode::Idiv(OperandNode::convert(right)?),
                    InstructionNode::Mov(
                        OperandNode::Reg(Register::AX), // read quotient result from EAX
                        OperandNode::convert(dst)?,
                    ),
                ]
                .into()),
                BirdsBinaryOperatorNode::Mod => Ok(vec![
                    InstructionNode::Mov(
                        OperandNode::convert(left)?,
                        OperandNode::Reg(Register::AX),
                    ),
                    InstructionNode::Cdq,
                    // note: idiv can't operate on immediate values, so we need to stuff those into
                    // a register during a later pass.
                    InstructionNode::Idiv(OperandNode::convert(right)?),
                    InstructionNode::Mov(
                        OperandNode::Reg(Register::DX), // read remainder result from EDX
                        OperandNode::convert(dst)?,
                    ),
                ]
                .into()),
                _ => Ok(vec![
                    InstructionNode::Mov(
                        OperandNode::convert(left)?,
                        OperandNode::convert(dst.clone())?,
                    ),
                    InstructionNode::Binary(
                        AssemblyBinaryOperatorNode::convert(op)?,
                        OperandNode::convert(right)?,
                        OperandNode::convert(dst)?,
                    ),
                ]
                .into()),
            },
        }
    }

    fn replace_mock_registers(
        &mut self,
        stack_locations: &mut HashMap<String, i32>,
        current_max_pointer: &mut i32,
    ) {
        match self {
            InstructionNode::Mov(src, dst) => {
                src.replace_mock_register(stack_locations, current_max_pointer);
                dst.replace_mock_register(stack_locations, current_max_pointer);
            }
            InstructionNode::Unary(_, dst) => {
                dst.replace_mock_register(stack_locations, current_max_pointer);
            }
            InstructionNode::Binary(_, src, dst) => {
                src.replace_mock_register(stack_locations, current_max_pointer);
                dst.replace_mock_register(stack_locations, current_max_pointer);
            }
            InstructionNode::Idiv(src) => {
                src.replace_mock_register(stack_locations, current_max_pointer);
            }
            _ => {}
        }
    }

    fn fix_instructions_with_two_stack_entries(&self) -> Option<VecDeque<InstructionNode>> {
        match self {
            InstructionNode::Mov(OperandNode::Stack(src), OperandNode::Stack(dst)) => Some(
                vec![
                    InstructionNode::Mov(OperandNode::Stack(*src), OperandNode::Reg(Register::R10)),
                    InstructionNode::Mov(OperandNode::Reg(Register::R10), OperandNode::Stack(*dst)),
                ]
                .into(),
            ),
            InstructionNode::Binary(op, OperandNode::Stack(src), OperandNode::Stack(dst)) => Some(
                vec![
                    InstructionNode::Mov(OperandNode::Stack(*src), OperandNode::Reg(Register::R10)),
                    InstructionNode::Binary(
                        op.clone(),
                        OperandNode::Reg(Register::R10),
                        OperandNode::Stack(*dst),
                    ),
                ]
                .into(),
            ),
            _ => None,
        }
    }

    fn fix_immediate_values_in_bad_places(&self) -> Option<VecDeque<InstructionNode>> {
        match self {
            InstructionNode::Idiv(OperandNode::Imm(value)) => Some(
                vec![
                    InstructionNode::Mov(
                        OperandNode::Imm(*value),
                        // we use r10d to fix src's
                        OperandNode::Reg(Register::R10),
                    ),
                    InstructionNode::Idiv(OperandNode::Reg(Register::R10)),
                ]
                .into(),
            ),
            _ => None,
        }
    }

    fn fix_memory_address_as_dst(&self) -> Option<VecDeque<InstructionNode>> {
        match self {
            InstructionNode::Binary(
                AssemblyBinaryOperatorNode::Mult,
                src,
                OperandNode::Stack(value),
            ) => Some(
                vec![
                    InstructionNode::Mov(
                        OperandNode::Stack(*value),
                        OperandNode::Reg(Register::R11),
                    ),
                    InstructionNode::Binary(
                        AssemblyBinaryOperatorNode::Mult,
                        src.clone(),
                        OperandNode::Reg(Register::R11), // we use r11d to fix dst's
                    ),
                    InstructionNode::Mov(
                        OperandNode::Reg(Register::R11),
                        OperandNode::Stack(*value),
                    ),
                ]
                .into(),
            ),
            _ => None,
        }
    }
}

impl OperandNode {
    fn convert(input: BirdsValueNode) -> Result<OperandNode, Box<dyn Error>> {
        match input {
            BirdsValueNode::Constant(Type::Integer(c)) => Ok(OperandNode::Imm(c)),
            BirdsValueNode::Var(s) => Ok(OperandNode::MockReg(s)),
        }
    }

    fn replace_mock_register(
        &mut self,
        stack_locations: &mut HashMap<String, i32>,
        current_max_pointer: &mut i32,
    ) {
        if let OperandNode::MockReg(name) = self {
            let new_location = stack_locations.entry(name.to_string()).or_insert_with(|| {
                *current_max_pointer -= 4;
                *current_max_pointer
            });
            *self = OperandNode::Stack(*new_location);
        }
    }
}

impl AssemblyUnaryOperatorNode {
    fn convert(input: BirdsUnaryOperatorNode) -> Result<AssemblyUnaryOperatorNode, Box<dyn Error>> {
        match input {
            BirdsUnaryOperatorNode::Negate => Ok(AssemblyUnaryOperatorNode::Neg),
            BirdsUnaryOperatorNode::Complement => Ok(AssemblyUnaryOperatorNode::Not),
        }
    }
}

impl AssemblyBinaryOperatorNode {
    fn convert(
        input: BirdsBinaryOperatorNode,
    ) -> Result<AssemblyBinaryOperatorNode, Box<dyn Error>> {
        match input {
            BirdsBinaryOperatorNode::Add => Ok(AssemblyBinaryOperatorNode::Add),
            BirdsBinaryOperatorNode::Subtract => Ok(AssemblyBinaryOperatorNode::Sub),
            BirdsBinaryOperatorNode::Multiply => Ok(AssemblyBinaryOperatorNode::Mult),
            BirdsBinaryOperatorNode::Divide | BirdsBinaryOperatorNode::Mod => {
                panic!("should not treat mod and divide as binary expressions during codegen")
            }
        }
    }
}
