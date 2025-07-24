use super::{
    BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, ExpressionNode, FunctionNode,
    ProgramNode, StatementNode, Type, UnaryOperatorNode,
};
use crate::compiler::IndentDisplay;
use std::{borrow::Borrow, fmt::Display};

impl Display for ProgramNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Program (\n{}\n)",
            self.function.fmt_indent(4, self.has_comments)
        )
    }
}

impl IndentDisplay for FunctionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        format!(
            // TODO: reformat when functions can have multiple statements
            "{:indent$}Function (\n{:indent$}    name: {},\n{:indent$}    body: {},\n{:indent$})",
            "",
            "",
            self.name,
            "",
            self.body.fmt_indent(indent + 8, comments),
            "",
            indent = indent,
        )
    }
}

impl IndentDisplay for Block {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        format!("\n{:indent$}", "", indent = indent)
            + &self
                .iter()
                .map(|item| match item {
                    BlockItemNode::Statement(s) => s.fmt_indent(indent, comments),
                    BlockItemNode::Declaration(d) => d.fmt_indent(indent, comments),
                })
                .collect::<Vec<String>>()
                .join(format!(",\n{:indent$}", "", indent = indent).as_str())
    }
}

impl IndentDisplay for StatementNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            StatementNode::Return(value) => {
                format!("Return {}", value.fmt_indent(indent + 4, comments),)
            }
            StatementNode::Expression(e) => e.fmt_indent(indent + 4, comments),
            StatementNode::Pass => "pass".to_string(),
            StatementNode::If(condition, then, otherwise) => {
                if let Some(other) = otherwise.borrow() {
                    format!(
                        "if ({})\n{:indent$}    {}\n{:indent$}else\n{:indent$}    {}",
                        condition.fmt_indent(indent + 4, comments),
                        "",
                        then.fmt_indent(indent + 4, comments),
                        "",
                        "",
                        other.fmt_indent(indent, comments),
                        indent = indent
                    )
                } else {
                    format!(
                        "if ({})\n{:indent$}    {}",
                        condition.fmt_indent(indent + 4, comments),
                        "",
                        then.fmt_indent(indent + 4, comments),
                        indent = indent
                    )
                }
            }
            StatementNode::Label(s, statement) => {
                format!("{}: {}", s, statement.fmt_indent(indent, comments))
            }
            StatementNode::Goto(s) => format!("goto {}", s),
        }
    }
}

impl IndentDisplay for DeclarationNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            DeclarationNode::Declaration(t, s, Some(e)) => {
                format!(
                    "{} {} = {}",
                    t.fmt_indent(indent, comments),
                    s,
                    e.fmt_indent(indent + 4, comments),
                )
            }
            DeclarationNode::Declaration(t, s, None) => {
                format!("{} {}", t.fmt_indent(indent, comments), s,)
            }
        }
    }
}

impl IndentDisplay for Type {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            Type::Integer => "int".to_string(),
        }
    }
}

impl IndentDisplay for ExpressionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            ExpressionNode::IntegerConstant(value) => value.to_string(),
            ExpressionNode::Unary(operator, exp) => {
                format!(
                    "({}{})",
                    operator.fmt_indent(indent + 4, comments),
                    exp.fmt_indent(indent + 4, comments),
                )
            }
            ExpressionNode::Binary(operator, left, right) => {
                format!(
                    "({} {} {})",
                    left.fmt_indent(indent + 4, comments),
                    operator.fmt_indent(indent + 4, comments),
                    right.fmt_indent(indent + 4, comments)
                )
            }
            ExpressionNode::Var(s) => format!("Var({})", s),
            ExpressionNode::Assignment(l, r) => format!(
                "{} = {}",
                l.fmt_indent(indent, comments),
                r.fmt_indent(indent, comments)
            ),
            ExpressionNode::Ternary(condition, then, otherwise) => {
                format!(
                    "{} ? {} : {}",
                    condition.fmt_indent(indent + 4, comments),
                    then.fmt_indent(indent + 4, comments),
                    otherwise.fmt_indent(indent, comments),
                )
            }
        }
    }
}

impl IndentDisplay for UnaryOperatorNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            UnaryOperatorNode::Complement => "~",
            UnaryOperatorNode::Negate => "-",
            UnaryOperatorNode::Not => "!",
            UnaryOperatorNode::PrefixIncrement => "++",
            UnaryOperatorNode::PrefixDecrement => "--",
            UnaryOperatorNode::SuffixIncrement => "(this is a suffix)++",
            UnaryOperatorNode::SuffixDecrement => "(this is a suffix)--",
        }
        .to_string()
    }
}

impl IndentDisplay for BinaryOperatorNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            BinaryOperatorNode::Add => "+",
            BinaryOperatorNode::Subtract => "-",
            BinaryOperatorNode::Multiply => "*",
            BinaryOperatorNode::Divide => "/",
            BinaryOperatorNode::Mod => "%",
            BinaryOperatorNode::And => "&&",
            BinaryOperatorNode::Or => "||",
            BinaryOperatorNode::Equal => "==",
            BinaryOperatorNode::NotEqual => "!=",
            BinaryOperatorNode::Less => "<",
            BinaryOperatorNode::Greater => ">",
            BinaryOperatorNode::LessEqual => "<=",
            BinaryOperatorNode::GreaterEqual => ">=",
            BinaryOperatorNode::BitwiseAnd => "&",
            BinaryOperatorNode::BitwiseXor => "^",
            BinaryOperatorNode::BitwiseOr => "|",
            BinaryOperatorNode::ShiftLeft => "<<",
            BinaryOperatorNode::ShiftRight => ">>",
        }
        .to_string()
    }
}
