use super::{
    birds::{
        BirdsInstructionNode, BirdsInstructions, BirdsProgramNode, BirdsUnaryOperatorNode,
        BirdsValueNode,
    },
    lexer::Type,
    IndentDisplay,
};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Display,
};

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

#[derive(Clone)]
struct Instructions(VecDeque<InstructionNode>);

#[derive(Clone)]
enum InstructionNode {
    Mov(OperandNode, OperandNode),
    Unary(AssemblyUnaryOperatorNode, OperandNode), // Operand here is both the src and dst.
    AllocateStack(i32),                            // number of bytes to allocate
    Custom(String),
    Ret,
}

#[derive(Clone)]
enum AssemblyUnaryOperatorNode {
    Neg,
    Not,
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
    AX,
    R10,
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
        let mut new_instructions: VecDeque<InstructionNode> = VecDeque::new();
        // push the address of RBP to the stack (at RSP).
        new_instructions.push_back(InstructionNode::Custom("pushq %rbp".to_string()));
        // move RBP to RSP's current location.
        new_instructions.push_back(InstructionNode::Custom("movq %rsp, %rbp".to_string()));
        // allocate space for local variables in the space before RSP in the stack.
        new_instructions.push_back(InstructionNode::AllocateStack(-current_max_pointer));
        for instruction in instructions.iter_mut() {
            if let Some(mut res) = instruction.fix_instructions_with_two_stack_entries() {
                new_instructions.append(&mut res);
            } else {
                new_instructions.push_back(instruction.clone())
            }
        }

        Ok(Instructions(new_instructions))
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
            BirdsInstructionNode::Binary(_, _, _, _) => todo!(),
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
            _ => {}
        }
    }

    fn fix_instructions_with_two_stack_entries(&mut self) -> Option<VecDeque<InstructionNode>> {
        match self {
            InstructionNode::Mov(src, dst) => {
                if let (&OperandNode::Stack(_), &OperandNode::Stack(_)) = (&src, &dst) {
                    Some(
                        vec![
                            InstructionNode::Mov(src.clone(), OperandNode::Reg(Register::R10)),
                            InstructionNode::Mov(OperandNode::Reg(Register::R10), dst.clone()),
                        ]
                        .into(),
                    )
                } else {
                    None
                }
            }
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

impl Display for AssemblyProgramNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}\n",
            self.function.fmt_indent(0, self.has_comments),
            self.footer.fmt_indent(4, self.has_comments)
        )
    }
}

impl IndentDisplay for AssemblyFunctionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        let function_name = if self.is_mac {
            "_".to_string() + &self.name
        } else {
            self.name.clone()
        };

        format!(
            "{}\n{:indent$}{}:\n{}",
            self.header.fmt_indent(indent + 4, comments),
            "",
            function_name,
            self.instructions.fmt_indent(indent + 4, comments),
            indent = indent
        )
    }
}

impl IndentDisplay for ExtraStrings {
    fn fmt_indent(&self, indent: usize, _comments: bool) -> String {
        let mut out = format!("{:indent$}", "", indent = indent);
        out += &self
            .0
            .join(format!("\n{:indent$}", "", indent = indent).as_str())
            .to_string();

        out
    }
}

impl IndentDisplay for Instructions {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        let mut out = format!("{:indent$}", "", indent = indent);
        out += &self
            .0
            .iter()
            .map(|instruction| instruction.fmt_indent(indent, comments))
            .collect::<Vec<String>>()
            .join(format!("\n{:indent$}", "", indent = indent).as_str())
            .to_string();

        out
    }
}

impl IndentDisplay for InstructionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            InstructionNode::Ret => {
                if comments {
                    format!(
                        "# return from this function\n{:indent$}# Restore RBP to where it was previously (whose location was stored at the current RBP), to free up the memory used by this function. popq reads the value at RSP.\n{:indent$}movq %rbp, %rsp\n{:indent$}popq %rbp\n{:indent$}ret",
                        "",
                        "",
                        "",
                        "",
                        indent = indent
                    )
                } else {
                    format!(
                        "movq %rbp, %rsp\n{:indent$}popq %rbp\n{:indent$}ret",
                        "",
                        "",
                        indent = indent
                    )
                }
            }
            InstructionNode::Mov(src, dst) => {
                if comments {
                    format!(
                        "# movl means move a 32 bit word (left) to a register (right)\n{:indent$}movl {}, {}",
                        "",
                        src,
                        dst,
                        indent = indent
                    )
                } else {
                    format!("movl {}, {}", src, dst)
                }
            }
            InstructionNode::Custom(s) => s.to_string(),
            InstructionNode::Unary(op, src) => format!("{} {}", op, src),
            InstructionNode::AllocateStack(num) => {
                if comments {
                    format!(
                    "# move the stack pointer by n bytes to the left. This has the effect of allocating n bytes for use in this function.\n{:indent$}# Variables are allocated relative to RBP, which is where RSP was also located before this operation\n{:indent$}subq ${}, %rsp",
                    "", "", num, indent=indent
                )
                } else {
                    format!("subq ${}, %rsp", num)
                }
            }
        }
    }
}

impl Display for OperandNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperandNode::Imm(int) => {
                write!(f, "${}", int)
            }
            OperandNode::Reg(reg) => match reg {
                Register::AX => write!(f, "%eax"),
                Register::R10 => write!(f, "%r10d"),
            },
            OperandNode::MockReg(name) => panic!(
                "Tried to generate assembly code with a mock register: {}",
                name
            ),
            OperandNode::Stack(num) => write!(f, "{}(%rbp)", num),
        }
    }
}

impl Display for AssemblyUnaryOperatorNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssemblyUnaryOperatorNode::Neg => write!(f, "negl"),
            AssemblyUnaryOperatorNode::Not => write!(f, "notl"),
        }
    }
}
