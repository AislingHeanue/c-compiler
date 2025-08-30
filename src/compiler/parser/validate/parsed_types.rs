use std::error::Error;

use crate::compiler::types::Type;

use super::{CheckTypes, ValidateContext};

impl CheckTypes for Type {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            Type::Function(out, params, _) => {
                out.check_types(context)?;
                for param in params {
                    param.check_types(context)?
                }
            }
            Type::Array(inner, _) => {
                if !inner.is_complete(&mut context.structs) {
                    return Err("Array can only be composed of complete types".into());
                }
                inner.check_types(context)?
            }
            Type::Pointer(inner, _) => inner.check_types(context)?,
            _ => {}
        }
        Ok(())
    }
}
