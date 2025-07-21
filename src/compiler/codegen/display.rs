use super::{
    AssemblyBinaryOperatorNode, AssemblyFunctionNode, AssemblyProgramNode,
    AssemblyUnaryOperatorNode, ExtraStrings, InstructionNode, Instructions, OperandNode, Register,
};
use crate::compiler::IndentDisplay;
use std::fmt::Display;

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
            InstructionNode::Binary(op, src, dst) => format!("{} {}, {}", op, src, dst),
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
            InstructionNode::Idiv(src) => {
                if comments {
                    format!(
                    "# Read the dividend from EDX and EAX, and then store the quotient in EDX and the remainder in EAX\n{:indent$}idivl {}",
                    "", src, indent=indent
                )
                } else {
                    format!("idivl {}", src)
                }
            }
            InstructionNode::Cdq => {
                if comments {
                    format!(
                    "# Extend the 32-bit value in EAX to a 64-bit value spanning EDX and EAX\n{:indent$}cdq",
                    "", indent=indent
                )
                } else {
                    "cdq".to_string()
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
                Register::DX => write!(f, "%edx"),
                Register::R11 => write!(f, "%r11d"),
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

impl Display for AssemblyBinaryOperatorNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssemblyBinaryOperatorNode::Add => write!(f, "addl"),
            AssemblyBinaryOperatorNode::Sub => write!(f, "subl"),
            AssemblyBinaryOperatorNode::Mult => write!(f, "imull"),
        }
    }
}
