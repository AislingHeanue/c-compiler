use std::error::Error;

use crate::compiler::{
    parser::VariableDeclaration,
    types::{
        ComparableStatic, InitialValue, StaticInitialiser, StorageClass, StorageInfo, SymbolInfo,
        Type,
    },
};

use super::{CheckTypes, Validate, ValidateContext, ValidationPass};

impl Validate for VariableDeclaration {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::TypeChecking) {
            // check the type of the declared variable always
            self.variable_type.check_types(context)?;
            if self.variable_type == Type::Void {
                return Err("Cannot declare a variable with a void type".into());
            }
            if let Some(ref mut struct_declaration) = self.struct_declaration {
                struct_declaration.check_types(context)?;
            }

            if context.current_function_name.is_none() {
                self.validate_file_scope(context)?;
            } else {
                self.validate_block_scope(context)?;
            }
        }
        self.init.validate(context)?;
        Ok(())
    }
}

impl VariableDeclaration {
    fn validate_file_scope(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let mut initial_value = if let Some(ref mut init) = self.init.as_mut() {
            init.validate(context)?;
            InitialValue::Initial(init.create_static_init_list(&self.variable_type, context)?)
        } else if matches!(self.storage_class, Some(StorageClass::Extern)) {
            InitialValue::None
        } else {
            InitialValue::Tentative
        };

        if initial_value != InitialValue::None
            && !self.variable_type.is_complete(&mut context.structs)
        {
            return Err("Cannot initialise a variable with an incomplete type".into());
        }

        let mut is_global = !matches!(self.storage_class, Some(StorageClass::Static));
        if let Some(old_symbol_info) = context.symbols.get(&self.name) {
            if self.variable_type != old_symbol_info.symbol_type {
                return Err(format!(
                    "Incompatible type for variable declarations: {:?}",
                    old_symbol_info.symbol_type,
                )
                .into());
            }
            if let StorageInfo::Static(old_init, was_global) = &old_symbol_info.storage {
                if matches!(self.storage_class, Some(StorageClass::Extern)) {
                    is_global = *was_global;
                } else if is_global != *was_global {
                    return Err("Conflicting variable linkage".into());
                }

                match old_init {
                    InitialValue::Initial(_) => {
                        if matches!(initial_value, InitialValue::Initial(_)) {
                            return Err("Conflicting file-scope variable definitions".into());
                        } else {
                            initial_value = old_init.clone();
                        }
                    }
                    InitialValue::Tentative => {
                        if !matches!(initial_value, InitialValue::Initial(_)) {
                            initial_value = InitialValue::Tentative;
                        }
                    }
                    _ => {}
                }
            } else {
                panic!(
                    "Incorrect storage type for variable: {:?}",
                    old_symbol_info.storage
                )
            }
        }
        context.symbols.insert(
            self.name.clone(),
            SymbolInfo {
                symbol_type: self.variable_type.clone(),
                storage: StorageInfo::Static(initial_value, is_global),
            },
        );
        Ok(())
    }

    fn validate_block_scope(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        match self.storage_class {
            Some(StorageClass::Extern) => {
                if self.init.is_some() {
                    return Err("Extern block-scope variable may not have an initialiser".into());
                }
                if let Some(old_symbol_info) = context.symbols.get(&self.name) {
                    if old_symbol_info.symbol_type != self.variable_type {
                        return Err("Variable redeclared with Incompatible type".into());
                    }
                } else {
                    context.symbols.insert(
                        self.name.clone(),
                        SymbolInfo {
                            symbol_type: self.variable_type.clone(),
                            storage: StorageInfo::Static(InitialValue::None, true),
                        },
                    );
                }
            }
            Some(StorageClass::Static) => {
                if !self.variable_type.is_complete(&mut context.structs) {
                    return Err("Cannot initialise a variable with an incomplete type".into());
                }
                // don't call check_types here, since we don't want to pollute constant expressions
                // with more complex expressions, which we may end up doing with other expressions
                let initial_value = match &mut self.init {
                    Some(ref mut initialiser) => {
                        initialiser.validate(context)?;
                        InitialValue::Initial(
                            initialiser.create_static_init_list(&self.variable_type, context)?,
                        )
                    }
                    None => {
                        let len = self.variable_type.get_size(&mut context.structs);
                        InitialValue::initial(StaticInitialiser::Comparable(
                            ComparableStatic::ZeroBytes(len),
                        ))
                    }
                };
                context.symbols.insert(
                    self.name.clone(),
                    SymbolInfo {
                        symbol_type: self.variable_type.clone(),
                        storage: StorageInfo::Static(initial_value, false),
                    },
                );
            }
            None => {
                if !self.variable_type.is_complete(&mut context.structs) {
                    return Err("Cannot initialise a variable with an incomplete type".into());
                }
                context.symbols.insert(
                    self.name.clone(),
                    SymbolInfo {
                        symbol_type: self.variable_type.clone(),
                        storage: StorageInfo::Automatic,
                    },
                );

                // cast the expression to the appropriate type at runtime
                if let Some(init) = &mut self.init {
                    init.check_types(&self.variable_type, context)?;
                }
            }
        }
        Ok(())
    }
}
