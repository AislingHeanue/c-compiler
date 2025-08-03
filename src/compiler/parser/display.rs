use itertools::Itertools;

use super::{
    BinaryOperatorNode, Block, BlockItemNode, CodeDisplay, Constant, DeclarationNode,
    DisplayContext, ExpressionNode, ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration,
    ProgramNode, StatementNode, Type, UnaryOperatorNode, VariableDeclaration,
};
use std::{borrow::Borrow, fmt::Display};

impl Display for ProgramNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // silly inclusion to make the output code more like Go.
        // writeln!(f, "package main")?;
        write!(
            f,
            "{}",
            self.declarations.show(&mut DisplayContext { indent: 0 })
        )
    }
}

impl CodeDisplay for Vec<DeclarationNode> {
    fn show(&self, context: &mut DisplayContext) -> String {
        self.iter().map(|f| f.show(context)).join("\n\n")
    }
}

impl CodeDisplay for Block {
    fn show(&self, context: &mut DisplayContext) -> String {
        // "\n".to_string()
        self.iter()
            .map(|item| match item {
                BlockItemNode::Statement(s) => s.show(context),
                BlockItemNode::Declaration(d) => {
                    context.new_line_start().to_string() + &d.show(context)
                }
            })
            .collect::<Vec<String>>()
            // .join(format!("\n{:indent$}", "", indent = context.indent).as_str())
            .join("")
    }
}

impl CodeDisplay for StatementNode {
    fn show(&self, context: &mut DisplayContext) -> String {
        let var_name = match self {
            StatementNode::Return(value) => {
                format!(
                    "{}return {}",
                    context.new_line_start(),
                    value.show(&mut context.indent()),
                )
            }
            StatementNode::Expression(e) => format!(
                "{}{}",
                context.new_line_start(),
                e.show(&mut context.indent())
            ),
            StatementNode::Pass => "pass".to_string(),
            StatementNode::If(condition, then, otherwise) => {
                if let Some(other) = otherwise.borrow() {
                    format!(
                        "{}if {} {} else {}",
                        context.new_line_start(),
                        condition.show(&mut context.indent()),
                        then.show(context),
                        other.show(context),
                    )
                } else {
                    format!(
                        "{}if {} {}",
                        context.new_line_start(),
                        condition.show(&mut context.indent()),
                        then.show(context),
                    )
                }
            }
            StatementNode::Label(s, statement) => {
                format!(
                    "{}{}: {}",
                    context.new_line_start(),
                    s,
                    statement.show(context)
                )
            }
            StatementNode::Goto(s) => format!("{}goto {}", context.new_line_start(), s),
            StatementNode::Compound(block) => {
                format!(
                    "{{{}{}}}",
                    block.show(&mut context.indent()),
                    context.new_line_start()
                )
            }
            StatementNode::Break(s) => {
                format!("{}break{}", context.new_line_start(), s.show(context))
            }
            StatementNode::Continue(s) => format!("continue{}", s.show(context)),
            StatementNode::While(expression, body, label) => {
                format!(
                    "{}while{} ({}) {}",
                    context.new_line_start(),
                    label.show(context),
                    expression.show(&mut context.indent()),
                    body.show(&mut context.indent())
                )
            }
            StatementNode::DoWhile(body, expression, label) => {
                format!(
                    "{}do{} {} while ({})",
                    context.new_line_start(),
                    label.show(context),
                    expression.show(&mut context.indent()),
                    body.show(&mut context.indent())
                )
            }
            StatementNode::For(init, cond, post, body, label) => {
                format!(
                    "{}for{}({}; {}; {}) {}",
                    context.new_line_start(),
                    label.show(context),
                    init.show(&mut context.indent()),
                    cond.show(&mut context.indent()),
                    post.show(&mut context.indent()),
                    body.show(context)
                )
            }
            StatementNode::Switch(expression, body, label, _) => format!(
                "{}switch{}({}) {}",
                context.new_line_start(),
                label.show(context),
                expression.show(&mut context.indent()),
                body.show(context)
            ),
            StatementNode::Case(expression, statement, _) => {
                *context = context.unindent();
                let out = format!(
                    "{}case {}:{}",
                    context.new_line_start(),
                    expression.show(context),
                    statement.show(&mut context.indent()),
                );
                *context = context.indent();
                out
            }
            StatementNode::Default(statement, _) => {
                *context = context.unindent();
                let out = format!(
                    "{}default:{}",
                    context.new_line_start(),
                    statement.show(&mut context.indent()),
                );
                *context = context.indent();
                out
            }
        };
        var_name
    }
}

impl CodeDisplay for ForInitialiserNode {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            ForInitialiserNode::Declaration(d) => d.show(context),
            ForInitialiserNode::Expression(e) => e.show(context),
        }
    }
}

impl CodeDisplay for VariableDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        if let Some(init) = &self.init {
            format!(
                "var {} {} = {}",
                self.name,
                self.variable_type.show(context),
                init.show(&mut context.indent()),
            )
        } else {
            format!("{} {}", self.variable_type.show(context), self.name,)
        }
    }
}

impl CodeDisplay for FunctionDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        let (out_type, param_types) = match &self.function_type {
            Type::Function(out, params) => (out, params),
            _ => unreachable!(),
        };
        let show_params = param_types
            .iter()
            .zip(self.params.iter())
            .map(|(t, p)| format!("{} {}", p, t.show(context)))
            .join(", ");

        if let Some(body) = &self.body {
            format!(
                "func {}({}) {} {{{}\n{:indent$}}}",
                // self.function_type.show(context),
                self.name,
                show_params,
                out_type.show(context),
                body.show(&mut context.indent()),
                "",
                indent = context.indent
            )
        } else {
            format!(
                "var {} func({}) {};",
                self.name,
                show_params,
                out_type.show(context),
            )
        }
    }
}

impl CodeDisplay for Vec<String> {
    fn show(&self, _context: &mut DisplayContext) -> String {
        self.iter().join(", ")
    }
}

impl CodeDisplay for DeclarationNode {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            DeclarationNode::Variable(v) => v.show(context),
            DeclarationNode::Function(f) => f.show(context),
        }
    }
}

impl CodeDisplay for Type {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            Type::Integer => "int32".to_string(),
            Type::Long => "int64".to_string(),
            Type::UnsignedInteger => "uint32".to_string(),
            Type::UnsignedLong => "uint64".to_string(),
            Type::Function(output, inputs) => {
                format!("func ({}) {}", inputs.show(context), output.show(context))
            }
            Type::Double => "float64".to_string(),
            Type::Pointer(t) => format!("*{}", t.show(context)),
        }
    }
}

impl CodeDisplay for Vec<Type> {
    fn show(&self, context: &mut DisplayContext) -> String {
        self.iter().map(|a| a.show(context)).join(", ")
    }
}

impl CodeDisplay for Option<String> {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            Some(s) => format!(" ({})", s),
            None => "".to_string(),
        }
    }
}

impl CodeDisplay for Option<ExpressionNode> {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            None => "".to_string(),
            Some(e) => e.show(context),
        }
    }
}

impl CodeDisplay for ExpressionNode {
    fn show(&self, context: &mut DisplayContext) -> String {
        if let Some(t) = &self.1 {
            format!("{} /* {} */", self.0.show(context), t.show(context))
        } else {
            self.0.show(context)
        }
    }
}

impl CodeDisplay for ExpressionWithoutType {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            ExpressionWithoutType::Constant(c) => c.show(context),
            ExpressionWithoutType::Unary(operator, exp) => {
                format!(
                    "{}{}",
                    operator.show(&mut context.indent()),
                    exp.show(&mut context.indent()),
                )
            }
            ExpressionWithoutType::Binary(operator, left, right) => {
                format!(
                    "({} {} {})",
                    left.show(&mut context.indent()),
                    operator.show(&mut context.indent()),
                    right.show(&mut context.indent())
                )
            }
            ExpressionWithoutType::Compound(op, l, r) => {
                format!(
                    "{} {}= {}",
                    l.show(&mut context.indent()),
                    op.show(&mut context.indent()),
                    r.show(&mut context.indent())
                )
            }
            ExpressionWithoutType::Var(s) => s.to_string(),
            ExpressionWithoutType::Assignment(l, r) => {
                format!("{} = {}", l.show(context), r.show(context))
            }
            ExpressionWithoutType::Ternary(condition, then, otherwise) => {
                format!(
                    "{} ? {} : {}",
                    condition.show(&mut context.indent()),
                    then.show(&mut context.indent()),
                    otherwise.show(context),
                )
            }
            ExpressionWithoutType::FunctionCall(name, args) => {
                format!("{}({})", name, args.show(&mut context.indent()))
            }
            ExpressionWithoutType::Cast(target_type, e) => {
                format!("{}({})", target_type.show(context), e.show(context))
            }
            ExpressionWithoutType::Dereference(e) => format!("*{}", e.show(context)),
            ExpressionWithoutType::AddressOf(e) => format!("&{}", e.show(context)),
        }
    }
}

impl CodeDisplay for Constant {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            Constant::Integer(c) => c.to_string(),
            Constant::Long(c) => format!("int64({})", c),
            Constant::UnsignedInteger(c) => format!("uint32({})", c),
            Constant::UnsignedLong(c) => format!("uint64({})", c),
            Constant::Double(c) => format!("float64({})", c),
        }
    }
}

impl CodeDisplay for Vec<ExpressionNode> {
    fn show(&self, context: &mut DisplayContext) -> String {
        self.iter().map(|e| e.show(context)).join(", ")
    }
}

impl CodeDisplay for UnaryOperatorNode {
    fn show(&self, _context: &mut DisplayContext) -> String {
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

impl CodeDisplay for BinaryOperatorNode {
    fn show(&self, _context: &mut DisplayContext) -> String {
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
