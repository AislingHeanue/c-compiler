use crate::preprocessor::lexer::PreprocessorToken;

enum ConditionNode {
    Binary(BinaryOp, Box<ConditionNode>, Box<ConditionNode>),
    Unary(UnaryOp, Box<ConditionNode>),
    Number(u64),
    Identifier(String),
    Defined(String),
    Ternary(Box<ConditionNode>, Box<ConditionNode>, Box<ConditionNode>),
}

enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    Xor,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

enum UnaryOp {
    Negate,
    Complement,
    Not,
}

// used for if statements
pub fn resolve_number(tokens: &Vec<PreprocessorToken>) -> i64 {
    // todo!()
    return 1;
}
