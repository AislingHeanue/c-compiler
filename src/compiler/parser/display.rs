use super::{
    BinaryOperatorNode, ExpressionNode, FunctionNode, ProgramNode, StatementNode, UnaryOperatorNode,
};
use crate::compiler::IndentDisplay;
use std::fmt::Display;

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
            self.body.fmt_indent(indent + 4, comments),
            "",
            indent = indent,
        )
    }
}

impl IndentDisplay for StatementNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            StatementNode::Return(value) => {
                format!("Return {}", value.fmt_indent(indent + 4, comments),)
            }
        }
    }
}

impl IndentDisplay for ExpressionNode {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String {
        match self {
            ExpressionNode::Constant(value) => value.fmt_indent(indent + 4, comments),
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
        }
    }
}

impl IndentDisplay for UnaryOperatorNode {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            UnaryOperatorNode::Complement => "~",
            UnaryOperatorNode::Negate => "-",
            UnaryOperatorNode::Not => "!",
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
        }
        .to_string()
    }
}
