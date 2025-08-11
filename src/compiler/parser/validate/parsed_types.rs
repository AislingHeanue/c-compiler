use std::error::Error;

use crate::compiler::types::Type;

use super::{CheckTypes, ValidateContext};

impl CheckTypes for Type {
    fn check_types(&mut self, _context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            Type::Function(out, params) => {
                out.check_types(_context)?;
                for param in params {
                    param.check_types(_context)?
                }
            }
            Type::Array(inner, _) => {
                if !inner.is_complete() {
                    return Err("Array can only be composed of complete types".into());
                }
                inner.check_types(_context)?
            }
            Type::Pointer(inner) => inner.check_types(_context)?,
            _ => {}
        }
        Ok(())
    }
}
