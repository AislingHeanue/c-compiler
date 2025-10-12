use itertools::Itertools;

use crate::compiler::types::EnumMember;

use super::{
    BinaryOperatorNode, BlockItemNode, BuiltinVa, Constant, DeclarationNode, EnumDeclaration,
    ExpressionNode, ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration,
    InitialiserNode, InitialiserWithoutType, InlineDeclaration, ProgramNode, StatementNode,
    StructDeclaration, StructMember, Type, TypeDeclaration, UnaryOperatorNode, VariableDeclaration,
};
use std::{borrow::Borrow, fmt::Display};

trait CodeDisplay {
    fn show(&self, context: &mut DisplayContext) -> String;
}

#[derive(Clone)]
pub struct DisplayContext {
    indent: usize,
}

impl DisplayContext {
    fn indent(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.indent += 4;
        s
    }
    fn unindent(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.indent -= 4;
        s
    }
    fn new_line_start(&self) -> String {
        format!("\n{:indent$}", "", indent = self.indent)
    }
}

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

impl CodeDisplay for Vec<BlockItemNode> {
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
                        "{}if {} {{{}{}}} else {{{}{}}}",
                        context.new_line_start(),
                        condition.show(&mut context.indent()),
                        then.show(&mut context.indent()),
                        context.new_line_start(),
                        other.show(&mut context.indent()),
                        context.new_line_start()
                    )
                } else {
                    format!(
                        "{}if {} {{{}{}}}",
                        context.new_line_start(),
                        condition.show(&mut context.indent()),
                        then.show(&mut context.indent()),
                        context.new_line_start()
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
                    "{}{{{}{}}}",
                    context.new_line_start(),
                    block.show(&mut context.indent()),
                    context.new_line_start()
                )
            }
            StatementNode::Break(s) => {
                format!("{}break{}", context.new_line_start(), s.show(context))
            }
            StatementNode::Continue(s) => format!("continue{}", s.show(context)),
            StatementNode::While(expression, body, _label) => {
                format!(
                    "{}while ({}) {}",
                    context.new_line_start(),
                    expression.show(&mut context.indent()),
                    body.show(&mut context.indent())
                )
            }
            StatementNode::DoWhile(body, expression, _label) => {
                format!(
                    "{}do {} while ({})",
                    context.new_line_start(),
                    expression.show(&mut context.indent()),
                    body.show(&mut context.indent())
                )
            }
            StatementNode::For(init, cond, post, body, _label) => {
                format!(
                    "{}for ({}; {}; {}) {}",
                    context.new_line_start(),
                    init.show(&mut context.indent()),
                    cond.show(&mut context.indent()),
                    post.show(&mut context.indent()),
                    body.show(context)
                )
            }
            StatementNode::Switch(expression, body, _label, _) => format!(
                "{}switch ({}) {}",
                context.new_line_start(),
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
            ForInitialiserNode::Declaration(ds) => ds
                .iter()
                .map(|d| d.show(context))
                .collect_vec()
                .show(context),
            ForInitialiserNode::Expression(e) => e.show(context),
        }
    }
}

impl CodeDisplay for VariableDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        let mut struct_part = "".to_string();
        for s in self.inline_declarations.iter() {
            struct_part += &(s.show(context) + &context.new_line_start())
        }
        if let Some(init) = &self.init {
            format!(
                "{}var {} {} = {}",
                struct_part,
                self.name,
                self.variable_type.show(context),
                init.show(&mut context.indent()),
            )
        } else {
            format!(
                "{}var {} {}",
                struct_part,
                self.name,
                self.variable_type.show(context)
            )
        }
    }
}

impl CodeDisplay for TypeDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        format!("type {} {}", self.name, self.target_type.show(context))
    }
}

impl CodeDisplay for InitialiserNode {
    fn show(&self, context: &mut DisplayContext) -> String {
        if let Some(t) = &self.1 {
            format!("{} /* {} */", self.0.show(context), t.show(context))
        } else {
            self.0.show(context)
        }
    }
}

impl CodeDisplay for InitialiserWithoutType {
    fn show(&self, context: &mut DisplayContext) -> String {
        match &self {
            InitialiserWithoutType::Single(e) => e.show(context),
            InitialiserWithoutType::Compound(initialisers) => format!(
                "{{{}}}",
                initialisers
                    .iter()
                    .map(|init| init.show(context))
                    .join(", "),
            ),
        }
    }
}

impl CodeDisplay for FunctionDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        let (out_type, param_types, is_variadic) = match &self.function_type {
            Type::Function(out, params, is_variadic) => (out, params, is_variadic),
            _ => unreachable!(),
        };
        let show_params = param_types
            .iter()
            .zip(self.params.iter())
            .map(|(t, p)| format!("{} {}", p, t.show(context)))
            .join(", ");

        if let Some(body) = &self.body {
            if *is_variadic {
                format!(
                    "func {}({}, ...) {} {{{}\n{:indent$}}}",
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
                    "func {}({}) {} {{{}\n{:indent$}}}",
                    // self.function_type.show(context),
                    self.name,
                    show_params,
                    out_type.show(context),
                    body.show(&mut context.indent()),
                    "",
                    indent = context.indent
                )
            }
        } else if *is_variadic {
            format!(
                "var {} func({}, ...) {};",
                self.name,
                show_params,
                out_type.show(context),
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
            DeclarationNode::Type(t) => t.show(context),
            DeclarationNode::Struct(s) => s.show(context),
            DeclarationNode::Enum(e) => e.show(context),
        }
    }
}

impl CodeDisplay for InlineDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            InlineDeclaration::Struct(s) => s.show(context),
            InlineDeclaration::Enum(e) => e.show(context),
        }
    }
}

impl CodeDisplay for StructDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        let word = if self.is_union { "union" } else { "struct" };
        if let Some(m) = &self.members {
            format!(
                "type {} {}{{{}{}}}",
                self.name,
                word,
                m.show(&mut context.indent()),
                context.new_line_start()
            )
        } else {
            format!("type {} {}{{?}}", self.name, word)
        }
    }
}

impl CodeDisplay for EnumDeclaration {
    fn show(&self, context: &mut DisplayContext) -> String {
        format!(
            "type {} enum{{{}{}}}",
            self.name,
            self.members.show(&mut context.indent()),
            context.new_line_start()
        )
    }
}

impl CodeDisplay for Type {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            Type::Integer => "int32".to_string(),
            Type::Long => "int64".to_string(),
            Type::UnsignedInteger => "uint32".to_string(),
            Type::UnsignedLong => "uint64".to_string(),
            Type::Function(output, inputs, is_variadic) => {
                if *is_variadic {
                    format!(
                        "func ({}, ...) {}",
                        inputs.show(context),
                        output.show(context)
                    )
                } else {
                    format!("func ({}) {}", inputs.show(context), output.show(context))
                }
            }
            Type::Float => "float32".to_string(),
            Type::Double => "float64".to_string(),
            Type::LongDouble => "longer_float64".to_string(),
            Type::Pointer(t, is_restricted) => {
                if *is_restricted {
                    format!("*restrict {}", t.show(context))
                } else {
                    format!("*{}", t.show(context))
                }
            }
            Type::Array(t, Some(s)) => format!("[{}]({})", s, t.show(context)),
            Type::Array(t, None) => format!("[]({})", t.show(context)),
            Type::Char => "rune".to_string(),
            Type::SignedChar => "signed_rune".to_string(),
            Type::UnsignedChar => "unsigned_rune".to_string(),
            Type::Void => "void".to_string(),
            Type::Struct(name, _) => name.to_string(),
            Type::Short => "int16".to_string(),
            Type::UnsignedShort => "uint16".to_string(),
            Type::LongLong => "longer_int64".to_string(),
            Type::UnsignedLongLong => "longer_uint64".to_string(),
            Type::Enum(v) => format!(
                "enum {{{}{}}}",
                v.show(&mut context.indent()),
                context.new_line_start()
            ),
        }
    }
}

impl CodeDisplay for Vec<StructMember> {
    fn show(&self, context: &mut DisplayContext) -> String {
        self.iter()
            .map(|member| {
                if let Some(name) = &member.name {
                    format!(
                        "{}{} {}",
                        context.new_line_start(),
                        name,
                        member.member_type.show(context)
                    )
                } else {
                    format!(
                        "{}{}",
                        context.new_line_start(),
                        member.member_type.show(context)
                    )
                }
            })
            .join("")
    }
}

impl CodeDisplay for Vec<EnumMember> {
    fn show(&self, context: &mut DisplayContext) -> String {
        self.iter()
            .map(|member| {
                format!(
                    "{}{} = {},",
                    context.new_line_start(),
                    if let Some(internal) = &member.internal_name {
                        internal
                    } else {
                        &member.name
                    },
                    member.init
                )
            })
            .join("")
            .trim_end_matches(",")
            .to_string()
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
            ExpressionWithoutType::Unary(operator, exp)
                if *operator == UnaryOperatorNode::SuffixIncrement
                    || *operator == UnaryOperatorNode::SuffixDecrement =>
            {
                format!(
                    "{}{}",
                    exp.show(&mut context.indent()),
                    operator.show(&mut context.indent()),
                )
            }
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
            ExpressionWithoutType::Compound(op, l, r, _) => {
                format!(
                    "({} {}= {})",
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
            ExpressionWithoutType::BuiltinFunctionCall(b) => b.show(context),
            ExpressionWithoutType::IndirectFunctionCall(left, args) => {
                format!(
                    "({})({})",
                    left.show(context),
                    args.show(&mut context.indent())
                )
            }
            ExpressionWithoutType::Cast(target_type, e, inline_declarations) => {
                let mut struct_part = "".to_string();
                for s in inline_declarations.iter() {
                    struct_part += &(s.show(context) + &context.new_line_start())
                }
                format!(
                    "{}{}({})",
                    struct_part,
                    target_type.show(context),
                    e.show(context)
                )
            }
            ExpressionWithoutType::Dereference(e) => format!("*{}", e.show(context)),
            ExpressionWithoutType::AddressOf(e) => format!("&{}", e.show(context)),
            ExpressionWithoutType::Subscript(src, inner) => {
                format!("{}[{}]", src.show(context), inner.show(context))
            }
            ExpressionWithoutType::String(s) => {
                let mut out = "\"".to_string();
                for c in s {
                    out += &c.show(context);
                }
                out += "\"";
                out
            }
            ExpressionWithoutType::SizeOf(e) => format!("sizeof({})", e.show(context)),
            ExpressionWithoutType::SizeOfType(t, struct_declarations) => {
                let mut struct_part = "".to_string();
                for s in struct_declarations.iter() {
                    struct_part += &(s.show(context) + &context.new_line_start())
                }
                format!("{}sizeof({})", struct_part, t.show(context))
            }
            ExpressionWithoutType::Dot(e, name) => format!("{}.{}", e.show(context), name),
            ExpressionWithoutType::Arrow(p, name) => format!("(*{}).{}", p.show(context), name),
        }
    }
}

impl CodeDisplay for i8 {
    fn show(&self, _context: &mut DisplayContext) -> String {
        char::from_u32(*self as u32)
            .map(|c| match c {
                '\n' => r"\n".to_string(),
                '\r' => r"\r".to_string(),
                '\\' => r"\\".to_string(),
                '\"' => r#"\""#.to_string(),
                _ => match c as u32 {
                    7 => r"\a".to_string(),
                    8 => r"\b".to_string(),
                    11 => r"\f".to_string(),
                    12 => r"\v".to_string(),
                    _ => c.to_string(),
                },
            })
            .unwrap_or("NULL".to_string())
    }
}

impl CodeDisplay for Constant {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            Constant::Integer(c) => c.to_string(),
            Constant::Long(c) => format!("int64({})", c),
            Constant::Short(c) => format!("int16({})", c),
            Constant::LongLong(c) => format!("longer_int64({})", c),
            Constant::UnsignedInteger(c) => format!("uint32({})", c),
            Constant::UnsignedLong(c) => format!("uint64({})", c),
            Constant::UnsignedShort(c) => format!("uint16({})", c),
            Constant::UnsignedLongLong(c) => format!("longer_uint64({})", c),
            Constant::Float(c) => format!("float32({})", c),
            Constant::Double(c) => format!("float64({})", c),
            Constant::LongDouble(c) => format!("longer_float64({})", c),
            Constant::Char(c) => format!("rune({})", c.show(context)),
            Constant::UnsignedChar(c) => format!("unsigned_rune({})", (*c as i8).show(context)),
            Constant::AddressOf(name) => format!("&{}", name),
        }
    }
}

impl CodeDisplay for BuiltinVa {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            BuiltinVa::Start(v, i) => format!(
                "__builtin_va_start({}, {})",
                v.show(context),
                i.show(context)
            ),
            BuiltinVa::Arg(v, t, inline_declarations) => {
                let mut struct_part = "".to_string();
                for s in inline_declarations.iter() {
                    struct_part += &(s.show(context) + &context.new_line_start())
                }
                format!(
                    "{}__builtin_va_arg({}, {})",
                    struct_part,
                    v.show(context),
                    t.show(context)
                )
            }
            BuiltinVa::End(v) => format!("__builtin_va_end({})", v.show(context)),
            BuiltinVa::Copy(dst, src) => format!(
                "__builtin_va_copy({}, {})",
                dst.show(context),
                src.show(context)
            ),
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
            UnaryOperatorNode::Identity => "+",
            UnaryOperatorNode::PrefixIncrement => "++",
            UnaryOperatorNode::PrefixDecrement => "--",
            UnaryOperatorNode::SuffixIncrement => "++",
            UnaryOperatorNode::SuffixDecrement => "--",
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
