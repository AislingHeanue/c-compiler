use std::{collections::HashMap, error::Error};

use crate::compiler::{
    parser::FunctionDeclaration,
    types::{StorageClass, StorageInfo, SymbolInfo, Type},
};

use super::{CheckTypes, Validate, ValidateContext, ValidationPass};

impl Validate for FunctionDeclaration {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::ReadLabels) {
            context.labels.insert(self.name.clone(), HashMap::new());
        }

        let previous_function_name = context.current_function_name.clone();
        context.current_function_name = Some(self.name.clone());

        if matches!(context.pass, ValidationPass::TypeChecking) {
            self.check_types(context)?;
        }

        self.body.validate(context)?;
        // reset current_function_name because it's an easy way to check if we are currently in a
        // block scope or file scope at the moment. May need to revisit this.
        context.current_function_name = previous_function_name;
        Ok(())
    }
}

impl CheckTypes for FunctionDeclaration {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        self.function_type.check_types(context)?;
        let mut is_defined = self.body.is_some();
        let mut is_global = !matches!(self.storage_class, Some(StorageClass::Static));

        let (return_type, arg_types) =
            if let Type::Function(ref return_type, ref mut arg_types) = &mut self.function_type {
                (return_type, arg_types)
            } else {
                return Err("Function is not a function type".into());
            };
        if matches!(**return_type, Type::Array(_, _)) {
            return Err("Function cannot return an array".into());
        }

        // bring any used structs into existence
        for s in self.struct_declarations.iter_mut() {
            s.check_types(context)?;
        }
        if is_defined
            && **return_type != Type::Void
            && !return_type.is_complete(&mut context.structs)
        {
            return Err("Function with body cannot return incomplete types".into());
        }

        // replace all array arg types in the function type with pointer types
        for arg in arg_types.iter_mut() {
            if arg == &Type::Void {
                return Err("Function cannot have void parameters".into());
            }
            if is_defined && !arg.is_complete(&mut context.structs) {
                return Err(
                    "Function with body cannot have parameters with incomplete types".into(),
                );
            }
            if let Type::Array(t, _size) = arg {
                *arg = Type::Pointer(t.clone(), false)
            };
        }

        // now that the type has been updated, drop the mutable reference to function_type
        let this_type = &self.function_type;
        let arg_types = if let Type::Function(_, ref arg_types) = this_type {
            arg_types
        } else {
            unreachable!()
        };

        if let Some(old_symbol_info) = context.symbols.get(&self.name) {
            if *this_type != old_symbol_info.symbol_type {
                return Err(format!(
                    "Incompatible types for function declarations: {:?} and {:?}",
                    old_symbol_info.symbol_type, this_type,
                )
                .into());
            }
            if let StorageInfo::Function(was_defined, was_global) = old_symbol_info.storage {
                if was_defined && self.body.is_some() {
                    return Err("Function is defined multiple times".into());
                }
                if was_global && !is_global {
                    return Err(
                        "Static function clashes with previously-declared non-static function"
                            .into(),
                    );
                }

                is_defined |= was_defined;
                is_global = was_global;
            } else {
                panic!(
                    "Incorrect storage type for function: {:?}",
                    old_symbol_info.storage
                )
            }
        }

        context.symbols.insert(
            self.name.clone(),
            SymbolInfo {
                symbol_type: this_type.clone(),
                storage: StorageInfo::Function(is_defined, is_global),
                constant: self.output_const,
                volatile: self.output_volatile,
            },
        );

        // add the parameters to the types map if this function has a body. This is done
        // because it allows those parameters to be type checked in the scope that they are
        // actually used (ie, params names from declarations which don't have bodies are thrown
        // out either way).
        // NOTE: PARAMS ARE ADDED TO THE SYMBOL MAP WITH TYPES HERE (this took ages to confirm)
        if self.body.is_some() {
            for (param, arg) in self.params.iter().zip(arg_types.iter()) {
                let converted_arg = match arg {
                    Type::Array(t, _size) => Type::Pointer(t.clone(), false),
                    _ => arg.clone(),
                };
                context.symbols.insert(
                    param.clone(),
                    SymbolInfo {
                        symbol_type: converted_arg,
                        storage: StorageInfo::Automatic,
                        constant: self.output_const,
                        volatile: self.output_volatile,
                    },
                );
            }
        };
        Ok(())
    }
}
