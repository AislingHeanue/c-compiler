use super::Type;
use crate::compiler::IndentDisplay;

impl IndentDisplay for Type {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            Type::Integer(value) => value.to_string(),
        }
    }
}
