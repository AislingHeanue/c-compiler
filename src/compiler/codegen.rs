use super::{
    lexer::Type,
    parser::{ExpressionNode, ProgramNode, StatementNode},
    IndentDisplay,
};
use std::{error::Error, fmt::Display};

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
struct Instructions(Vec<InstructionNode>);

#[derive(Clone)]
enum InstructionNode {
    Mov(OperandNode, OperandNode),
    Ret,
}

#[derive(Clone)]
enum OperandNode {
    Imm(i32),
    Register,
}

pub fn codegen(
    parsed: ProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
) -> Result<AssemblyProgramNode, Box<dyn Error>> {
    AssemblyProgramNode::convert(parsed, comments, linux, mac)
}

impl AssemblyProgramNode {
    fn convert(
        parsed: ProgramNode,
        comments: bool,
        linux: bool,
        mac: bool,
    ) -> Result<AssemblyProgramNode, Box<dyn Error>> {
        let StatementNode::Return(value) = parsed.function.body;
        let ExpressionNode::Constant(Type::Integer(int_value)) = value;
        let return_value = OperandNode::Imm(int_value);

        Ok(AssemblyProgramNode {
            function: AssemblyFunctionNode {
                header: if comments {
                    ExtraStrings(vec![
                        "# make the function name accessible globally".to_string(),
                        format!(".globl {}", parsed.function.name.value),
                    ])
                } else {
                    ExtraStrings(vec![format!(".globl {}", parsed.function.name.value)])
                },
                name: parsed.function.name.value,
                instructions: Instructions(vec![
                    InstructionNode::Mov(return_value, OperandNode::Register),
                    InstructionNode::Ret,
                ]),
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

impl Display for AssemblyProgramNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
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
            Self::Ret => {
                if comments {
                    format!(
                        "# return from this function\n{:indent$}ret",
                        "",
                        indent = indent
                    )
                } else {
                    "ret".to_string()
                }
            }
            Self::Mov(src, dst) => {
                if comments {
                    format!(
                    "# movl means move a 32 bit word (left) to a register (right)\n{:indent$}movl {}, {}",
                    "", src, dst, indent=indent
                )
                } else {
                    format!("movl {}, {}", src, dst)
                }
            }
        }
    }
}

impl Display for OperandNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(int) => {
                write!(f, "${}", int)
            }
            Self::Register => {
                write!(f, "%eax")
            }
        }
    }
}
