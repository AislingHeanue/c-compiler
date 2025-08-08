use std::{collections::HashMap, error::Error};

use itertools::process_results;

use crate::compiler::types::{InitialValue, StaticInitialiser};

use super::{
    BinaryOperatorNode, Block, BlockItemNode, ComparableStatic, Constant, DeclarationNode,
    ExpressionNode, ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration,
    InitialiserNode, InitialiserWithoutType, ProgramNode, StatementNode, StorageClass, StorageInfo,
    SwitchMapKey, SymbolInfo, Type, UnaryOperatorNode, VariableDeclaration,
};

trait Validate
where
    Self: Sized,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
}

#[derive(Clone, Debug)]
enum ValidationPass {
    // variable resolution is covered by parse.rs
    ReadLabels,
    ValidateLabels,
    LabelLoops,
    TypeChecking,
    // type checking needs to occur before this step, to make sure all array vars decay to
    // an array pointer, which is a variable type  that we can't assign to as 'AddressOf'
    // is not an lvalue.
    CheckLvalues,
}

#[derive(Debug)]
struct ValidateContext {
    pass: ValidationPass,
    // Function name -> user-defined name -> label name for birds
    num_labels: usize,
    num_loops: usize,
    num_switches: usize,
    num_switch_labels: usize,
    num_strings: usize,
    labels: HashMap<String, HashMap<String, String>>,
    current_function_name: Option<String>,
    // all loops + switch
    current_enclosing_loop_name_for_break: Option<String>,
    // all loops (not switch)
    current_enclosing_loop_name_for_case: Option<String>,
    current_enclosing_loop_name_for_continue: Option<String>,
    current_switch_labels: Option<HashMap<SwitchMapKey, String>>,
    current_switch_type: Option<Type>,
    symbols: HashMap<String, SymbolInfo>,
}

pub fn do_validate(
    parsed: &mut ProgramNode,
) -> Result<HashMap<String, SymbolInfo>, Box<dyn Error>> {
    let passes: Vec<ValidationPass> = vec![
        ValidationPass::ReadLabels,
        ValidationPass::ValidateLabels,
        ValidationPass::LabelLoops,
        ValidationPass::TypeChecking,
        ValidationPass::CheckLvalues,
    ];
    let mut validate_context = ValidateContext {
        pass: passes.first().unwrap().clone(),
        num_labels: 0,
        num_loops: 0,
        num_switches: 0,
        num_switch_labels: 0,
        num_strings: 0,
        labels: HashMap::new(),
        current_function_name: None,
        current_enclosing_loop_name_for_break: None,
        current_enclosing_loop_name_for_case: None,
        current_enclosing_loop_name_for_continue: None,
        current_switch_labels: None,
        current_switch_type: None,
        symbols: HashMap::new(),
    };
    for pass in passes {
        validate_context.pass = pass;
        parsed.validate(&mut validate_context)?;
    }

    Ok(validate_context.symbols)
}

impl<T> Validate for Option<T>
where
    T: Validate,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if let Some(e) = self {
            e.validate(context)?;
        }
        Ok(())
    }
}

impl<T> Validate for Vec<T>
where
    T: Validate,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        for i in self {
            i.validate(context)?;
        }
        Ok(())
    }
}

impl Validate for ProgramNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        for d in self.declarations.iter_mut() {
            d.validate(context)?;
        }
        Ok(())
    }
}

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

impl FunctionDeclaration {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let (return_type, arg_types) =
            if let Type::Function(ref return_type, ref mut arg_types) = &mut self.function_type {
                (return_type, arg_types)
            } else {
                return Err("Function is not a function type".into());
            };
        if matches!(**return_type, Type::Array(_, _)) {
            return Err("Function cannot return an array".into());
        }

        // replace all array arg types in the function type with pointer types
        for arg in arg_types.iter_mut() {
            if let Type::Array(t, _size) = arg {
                *arg = Type::Pointer(t.clone())
            };
        }

        // now that the type has been updated, drop the mutable reference to function_type
        let this_type = &self.function_type;
        let arg_types = if let Type::Function(_, ref arg_types) = this_type {
            arg_types
        } else {
            unreachable!()
        };

        let mut is_defined = self.body.is_some();
        let mut is_global = !matches!(self.storage_class, Some(StorageClass::Static));

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
                    Type::Array(t, _size) => Type::Pointer(t.clone()),
                    _ => arg.clone(),
                };
                context.symbols.insert(
                    param.clone(),
                    SymbolInfo {
                        symbol_type: converted_arg,
                        storage: StorageInfo::Automatic,
                    },
                );
            }
        };
        Ok(())
    }
}

impl Validate for VariableDeclaration {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::TypeChecking) {
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

impl Validate for InitialiserNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self.0 {
            InitialiserWithoutType::Single(ref mut e) => {
                e.validate(context)?;
            }
            InitialiserWithoutType::Compound(ref mut initialisers) => {
                for i in initialisers {
                    i.validate(context)?;
                }
            }
        }
        Ok(())
    }
}

impl InitialiserNode {
    fn create_static_init_list(
        &mut self,
        target_type: &Type,
        context: &mut ValidateContext,
    ) -> Result<Vec<StaticInitialiser>, Box<dyn Error>> {
        let init_list: Vec<StaticInitialiser> = match (target_type, &mut self.0) {
            (Type::Pointer(t), InitialiserWithoutType::Single(init_e))
                if init_e.is_string_literal() =>
            {
                if **t != Type::Char {
                    return Err("Can't initialise a non-character pointer with a string".into());
                }
                if let ExpressionWithoutType::String(s) = &init_e.0 {
                    context.num_strings += 1;
                    let new_name = format!("string.constant.{}", context.num_strings);
                    context.symbols.insert(
                        new_name.clone(),
                        SymbolInfo {
                            symbol_type: Type::Array(t.clone(), s.len() as u64 + 1),
                            storage: StorageInfo::Constant(StaticInitialiser::Ordinal(
                                ComparableStatic::String(s.to_vec(), true),
                            )),
                        },
                    );
                    vec![StaticInitialiser::Ordinal(ComparableStatic::Pointer(
                        new_name,
                    ))]
                } else {
                    unreachable!()
                }
            }

            (Type::Array(t, size), InitialiserWithoutType::Single(init_e)) => {
                if let ExpressionWithoutType::String(s) = &init_e.0 {
                    if s.len() > (*size).try_into().unwrap() {
                        return Err("Static String is too long".into());
                    }
                    let difference: i32 = *size as i32 - s.len() as i32;

                    if !t.is_character() {
                        return Err("Can't initialise a non-character array with a string".into());
                    }
                    let mut out = vec![StaticInitialiser::Ordinal(ComparableStatic::String(
                        s.clone(),
                        difference > 0,
                    ))];
                    if difference > 1 {
                        out.push(StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                            difference - 1,
                        )))
                    }
                    out
                } else {
                    return Err("Cannot initialise a static array with a scalar type".into());
                }
            }
            (_, InitialiserWithoutType::Single(ref i)) => {
                if matches!(target_type, Type::Pointer(_)) && !i.equals_null_pointer() {
                    return Err("Cannot initialise a static pointer with a non-pointer type".into());
                }
                let c = if let ExpressionWithoutType::Constant(c) = &i.0 {
                    c
                } else {
                    return Err(
                        "Static variables must be initialised with a constant expression".into(),
                    );
                };

                vec![c.convert_to(target_type)]
            }
            (Type::Array(t, size), InitialiserWithoutType::Compound(ref mut initialisers)) => {
                if initialisers.len() > (*size).try_into().unwrap() {
                    return Err("Too many initialisers in static declaration".into());
                }
                let mut statics: Vec<StaticInitialiser> = process_results(
                    initialisers
                        .iter_mut()
                        .map(|init| init.create_static_init_list(t, context)),
                    |iter| iter.flatten().collect(),
                )?;
                if initialisers.len() < (*size).try_into().unwrap() {
                    let offset = *size as i32 - initialisers.len() as i32;
                    statics.push(StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                        t.get_size() * offset,
                    )));
                }
                statics
            }
            (_, InitialiserWithoutType::Compound(_)) => {
                return Err("Only arrays can be initialised with a compound initialiser".into())
            }
        };
        Ok(init_list)
    }

    fn check_types(
        &mut self,
        target_type: &Type,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        self.1 = Some(target_type.clone());
        match (target_type, &mut self.0) {
            (Type::Array(t, size), InitialiserWithoutType::Single(ref mut init_e))
                if matches!(&init_e.0, ExpressionWithoutType::String(_s)) =>
            {
                if let ExpressionWithoutType::String(s) = &init_e.0 {
                    if !t.is_character() {
                        return Err(
                            "Can't initialise non-character array with a string literal".into()
                        );
                    }
                    if s.len() > (*size).try_into().unwrap() {
                        return Err("Initialiser has the wrong number of elements".into());
                    }
                } else {
                    unreachable!()
                }
            }

            (_, InitialiserWithoutType::Single(ref mut init_e)) => {
                init_e.check_types_and_convert(context)?;
                init_e.convert_type_by_assignment(target_type)?;
            }
            (Type::Array(t, size), InitialiserWithoutType::Compound(ref mut c_init)) => {
                if c_init.len() > (*size).try_into().unwrap() {
                    return Err("Initialiser has the wrong number of elements".into());
                }
                for init in c_init.iter_mut() {
                    init.check_types(t, context)?;
                }
                while c_init.len() < (*size).try_into().unwrap() {
                    c_init.push(InitialiserNode::zero(t));
                }
            }
            (_, InitialiserWithoutType::Compound(_)) => {
                return Err("Compound initialiser can only be used for arrays".into());
            }
        }
        Ok(())
    }

    fn zero(target_type: &Type) -> InitialiserNode {
        match target_type {
            Type::Integer => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Integer(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Long => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Long(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedInteger => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedInteger(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedLong => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Double => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Double(0.)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Array(t, size) => InitialiserNode(
                InitialiserWithoutType::Compound(
                    (0..*size).map(|_| InitialiserNode::zero(t)).collect(),
                ),
                Some(target_type.clone()),
            ),
            Type::Function(_, _) => unreachable!(),
            Type::Pointer(_) => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Char => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Char(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::SignedChar => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Char(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedChar => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedChar(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
        }
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
                        let len = self.variable_type.get_size();
                        InitialValue::initial(StaticInitialiser::Ordinal(
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

impl Validate for Block {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        for i in self {
            match i {
                BlockItemNode::Statement(s) => s.validate(context)?,
                BlockItemNode::Declaration(d) => d.validate(context)?,
            }
        }
        Ok(())
    }
}

impl Validate for DeclarationNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            DeclarationNode::Variable(v) => v.validate(context)?,
            DeclarationNode::Function(f) => f.validate(context)?,
        };
        Ok(())
    }
}

impl Validate for StatementNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::TypeChecking) {
            self.check_types(context)?;
        }
        match context.pass {
            ValidationPass::ReadLabels => {
                self.read_labels(context)?;
            }
            ValidationPass::ValidateLabels => {
                self.validate_labels(context)?;
            }
            // LabelLoops covers the context required by
            // all loops, switch, break, continue, case, default
            ValidationPass::LabelLoops => {
                self.label_loops(context)?;
            }
            ValidationPass::CheckLvalues => {}
            ValidationPass::TypeChecking => {}
        }
        match self {
            StatementNode::Expression(ref mut e) => e.validate(context)?,
            StatementNode::Pass => {}
            StatementNode::Return(ref mut e) => e.validate(context)?,
            StatementNode::If(ref mut condition, ref mut then, ref mut otherwise) => {
                condition.validate(context)?;
                then.validate(context)?;
                otherwise.validate(context)?;
            }
            StatementNode::Label(_, ref mut statement) => {
                statement.validate(context)?;
            }
            StatementNode::Goto(_) => {}
            StatementNode::Compound(ref mut block) => {
                block.validate(context)?;
            }
            StatementNode::Break(_) => {}
            StatementNode::Continue(_) => {}
            StatementNode::While(ref mut expression, ref mut body, ref mut label) => {
                if matches!(context.pass, ValidationPass::LabelLoops) {
                    let previous = StatementNode::enter_loop(context);
                    *label = context.current_enclosing_loop_name_for_break.clone();

                    expression.validate(context)?;
                    body.validate(context)?;

                    StatementNode::leave_loop(previous, context)
                } else {
                    expression.validate(context)?;
                    body.validate(context)?;
                };
            }
            StatementNode::DoWhile(ref mut body, ref mut expression, ref mut label) => {
                if matches!(context.pass, ValidationPass::LabelLoops) {
                    let previous = StatementNode::enter_loop(context);
                    *label = context.current_enclosing_loop_name_for_break.clone();

                    body.validate(context)?;
                    expression.validate(context)?;

                    StatementNode::leave_loop(previous, context)
                } else {
                    body.validate(context)?;
                    expression.validate(context)?;
                };
            }
            StatementNode::For(
                ref mut init,
                ref mut cond,
                ref mut post,
                ref mut body,
                ref mut label,
            ) => {
                if matches!(context.pass, ValidationPass::LabelLoops) {
                    let previous = StatementNode::enter_loop(context);
                    *label = context.current_enclosing_loop_name_for_break.clone();

                    init.validate(context)?;
                    cond.validate(context)?;
                    post.validate(context)?;
                    body.validate(context)?;

                    StatementNode::leave_loop(previous, context)
                } else {
                    init.validate(context)?;
                    cond.validate(context)?;
                    post.validate(context)?;
                    body.validate(context)?;
                };
            }
            StatementNode::Switch(ref mut expression, ref mut body, ref mut label, ref mut map) => {
                match context.pass {
                    ValidationPass::LabelLoops => {
                        let previous = StatementNode::enter_switch(context);
                        *label = context.current_enclosing_loop_name_for_break.clone();

                        expression.validate(context)?;
                        body.validate(context)?;

                        StatementNode::leave_switch(previous, context)
                    }
                    ValidationPass::TypeChecking => {
                        expression.validate(context)?;
                        expression.promote(context)?;
                        expression.check_types_and_convert(context)?;
                        let previous =
                            StatementNode::enter_switch_type_checking(expression, context);

                        if !expression.1.as_ref().unwrap().is_integer() {
                            return Err("Cannot Switch on non-integer type".into());
                        }
                        body.validate(context)?;
                        // only steal the map from the context after the switch body has been type
                        // checked
                        *map = context.current_switch_labels.take();
                        StatementNode::leave_switch_type_checking(previous, context);
                    }
                    _ => {
                        expression.validate(context)?;
                        body.validate(context)?;
                    }
                }
            }
            StatementNode::Case(expression, body, _) => {
                expression.validate(context)?;
                body.validate(context)?;
            }
            StatementNode::Default(body, _) => body.validate(context)?,
        };
        Ok(())
    }
}

impl StatementNode {
    fn read_labels(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if let StatementNode::Label(ref mut s, _) = self {
            let labels_defined = context
                .labels
                .get_mut(&context.current_function_name.clone().unwrap())
                .unwrap();

            if labels_defined.contains_key(s) {
                return Err(format!("Duplicate label name definition: {}", s).into());
            }
            context.num_labels += 1;
            let new_name = format!("{}_user_{}", s, context.num_labels);
            labels_defined.insert(s.clone(), new_name.clone());
            *s = new_name;
        }
        Ok(())
    }

    fn validate_labels(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if let StatementNode::Goto(ref mut name) = self {
            let labels_defined = context
                .labels
                .get_mut(&context.current_function_name.clone().unwrap())
                .unwrap();
            *name = labels_defined
                .get(name)
                .ok_or(format!("Label not found: {}", name))?
                .to_string();
        }

        Ok(())
    }

    fn label_loops(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            StatementNode::Break(ref mut label) => {
                *label = Some(
                    context
                        .current_enclosing_loop_name_for_break
                        .clone()
                        .ok_or::<Box<dyn Error>>("Break is not inside a loop or switch".into())?,
                );
            }
            StatementNode::Continue(ref mut label) => {
                *label = Some(
                    context
                        .current_enclosing_loop_name_for_continue
                        .clone()
                        .ok_or::<Box<dyn Error>>("Continue is not inside a loop".into())?,
                );
            }
            StatementNode::Case(_, _, ref mut label) => {
                let switch_label = context
                    .current_enclosing_loop_name_for_case
                    .clone()
                    .ok_or::<Box<dyn Error>>("Case is not inside a switch".into())?;

                context.num_switch_labels += 1;
                *label = format!("{}_case_{}", switch_label, context.num_switch_labels).into();
            }
            StatementNode::Default(_, ref mut label) => {
                let switch_label = context
                    .current_enclosing_loop_name_for_case
                    .clone()
                    .ok_or::<Box<dyn Error>>("Default is not inside a switch".into())?;

                *label = format!("{}_default", switch_label).into();
            }
            _ => {}
        };
        Ok(())
    }

    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            StatementNode::Return(e) => {
                e.check_types_and_convert(context)?;
                if let Type::Function(out, _) = &context
                    .symbols
                    .get(context.current_function_name.as_ref().unwrap())
                    .unwrap()
                    .symbol_type
                {
                    e.convert_type_by_assignment(out)?;
                } else {
                    unreachable!();
                };
            }
            StatementNode::Case(ref mut e, _, ref label) => {
                e.check_types_and_convert(context)?;

                let target_type = context
                    .current_switch_type
                    .clone()
                    .ok_or::<Box<dyn Error>>("Case is not in a switch statement".into())?;

                let constant_value = if let ExpressionWithoutType::Constant(ref mut c) = e.0 {
                    c.convert_ordinal_to(&target_type)
                } else {
                    return Err("Non constant expression found in case statement".into());
                };
                let ordinal = if let StaticInitialiser::Ordinal(ref o) = constant_value {
                    o
                } else {
                    return Err("Non ordinal (eg Double) expression found in case statement".into());
                };

                let already_present = context
                    .current_switch_labels
                    .as_mut()
                    .ok_or("Switch label map not found")?
                    .insert(
                        SwitchMapKey::Constant(ordinal.clone()),
                        label.as_ref().unwrap().clone(),
                    );

                if already_present.is_some() {
                    return Err(format!(
                        "Duplicate case expression in switch: {:?}",
                        constant_value
                    )
                    .into());
                }
            }
            StatementNode::Default(_, label) => {
                let already_present = context
                    .current_switch_labels
                    .as_mut()
                    .ok_or("Switch label map not found")?
                    .insert(SwitchMapKey::Default, label.clone().unwrap());

                if already_present.is_some() {
                    return Err("Duplicate default in switch".into());
                }
            }
            _ => (),
        };
        Ok(())
    }

    fn enter_loop(context: &mut ValidateContext) -> (Option<String>, Option<String>) {
        context.num_loops += 1;
        let new_loops_name = format!("loop_{}", context.num_loops);

        let previous_loop_name_for_break = context.current_enclosing_loop_name_for_break.clone();
        context.current_enclosing_loop_name_for_break = Some(new_loops_name.clone());

        let previous_loop_name_for_continue =
            context.current_enclosing_loop_name_for_continue.clone();
        context.current_enclosing_loop_name_for_continue = Some(new_loops_name);

        (
            previous_loop_name_for_break,
            previous_loop_name_for_continue,
        )
    }

    fn enter_switch(context: &mut ValidateContext) -> (Option<String>, Option<String>) {
        context.num_switches += 1;
        let new_switch_name = format!("switch_{}", context.num_switches);

        let previous_loop_name_for_break = context.current_enclosing_loop_name_for_break.clone();
        context.current_enclosing_loop_name_for_break = Some(new_switch_name.clone());

        let previous_loop_name_for_case = context.current_enclosing_loop_name_for_case.clone();
        context.current_enclosing_loop_name_for_case = Some(new_switch_name);

        (previous_loop_name_for_break, previous_loop_name_for_case)
    }

    fn enter_switch_type_checking(
        e: &ExpressionNode,
        context: &mut ValidateContext,
    ) -> (Option<Type>, Option<HashMap<SwitchMapKey, String>>) {
        let previous_switch_labels = context.current_switch_labels.take();
        context.current_switch_labels = Some(HashMap::new());

        let previous_switch_type = context.current_switch_type.take();
        context.current_switch_type = Some(e.1.as_ref().unwrap().clone());

        (previous_switch_type, previous_switch_labels)
    }

    fn leave_loop(previous_names: (Option<String>, Option<String>), context: &mut ValidateContext) {
        context.current_enclosing_loop_name_for_break = previous_names.0;
        context.current_enclosing_loop_name_for_continue = previous_names.1;
    }

    fn leave_switch(
        previous_names: (Option<String>, Option<String>),
        context: &mut ValidateContext,
    ) {
        context.current_enclosing_loop_name_for_break = previous_names.0;
        context.current_enclosing_loop_name_for_case = previous_names.1;
    }

    fn leave_switch_type_checking(
        previous: (Option<Type>, Option<HashMap<SwitchMapKey, String>>),
        context: &mut ValidateContext,
    ) {
        context.current_switch_type = previous.0;
        context.current_switch_labels = previous.1;
    }
}

impl Validate for ForInitialiserNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(v) => v.validate(context)?,
            ForInitialiserNode::Expression(e) => e.validate(context)?,
        }
        Ok(())
    }
}

impl Validate for ExpressionNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::CheckLvalues) {
            self.validate_lvalues(context)?;
        }
        if matches!(context.pass, ValidationPass::TypeChecking) {
            self.check_types(context)?;
        }

        match self.0 {
            ExpressionWithoutType::Constant(_) => {}
            ExpressionWithoutType::Unary(_, ref mut src) => {
                src.validate(context)?;
            }
            ExpressionWithoutType::Compound(_, ref mut left, ref mut right) => {
                left.validate(context)?;
                right.validate(context)?;
            }
            ExpressionWithoutType::Binary(_, ref mut left, ref mut right) => {
                left.validate(context)?;
                right.validate(context)?;
            }
            ExpressionWithoutType::Var(_) => {}
            ExpressionWithoutType::Subscript(ref mut src, ref mut inner) => {
                src.validate(context)?;
                inner.validate(context)?;
            }
            ExpressionWithoutType::Assignment(ref mut dst, ref mut src) => {
                dst.validate(context)?;
                src.validate(context)?;
            }
            ExpressionWithoutType::Ternary(ref mut condition, ref mut then, ref mut otherwise) => {
                condition.validate(context)?;
                then.validate(context)?;
                otherwise.validate(context)?;
            }
            ExpressionWithoutType::FunctionCall(_, ref mut params) => {
                params.validate(context)?;
            }
            ExpressionWithoutType::Cast(_, ref mut expression) => {
                expression.validate(context)?;
            }
            ExpressionWithoutType::Dereference(ref mut ptr) => {
                ptr.validate(context)?;
            }
            ExpressionWithoutType::AddressOf(ref mut object) => {
                object.validate(context)?;
            }
            ExpressionWithoutType::String(ref mut _s) => {}
        };
        Ok(())
    }
}

impl ExpressionNode {
    fn validate_lvalues(&mut self, _context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match &self.0 {
            ExpressionWithoutType::Unary(
                UnaryOperatorNode::PrefixIncrement
                | UnaryOperatorNode::PrefixDecrement
                | UnaryOperatorNode::SuffixIncrement
                | UnaryOperatorNode::SuffixDecrement,
                src,
            ) => {
                // VALIDATION: Make sure ++ and -- only operate on variables, not constants or
                // other expressions
                if !src.0.match_lvalue() {
                    return Err(format!(
                        "Can't perform increment/decrement operation on non-variable: {:?}",
                        src,
                    )
                    .into());
                }
            }
            ExpressionWithoutType::Assignment(ref dst, _) => {
                if !dst.0.match_lvalue() {
                    return Err(format!("Can't assign to non-variable: {:?}", dst).into());
                }
            }
            ExpressionWithoutType::Compound(_, ref left, _) => {
                if !left.0.match_lvalue() {
                    return Err(format!("Can't assign to non-variable: {:?}", left).into());
                }
            }
            _ => {}
        }
        Ok(())
    }

    // See System V ABI list of rules for how to reconcile types of various sizes
    // C Spec 6.3.1.8, Paragraph 1
    fn get_common_type(e1: &ExpressionNode, e2: &ExpressionNode) -> Type {
        let t1 = e1.1.as_ref().unwrap();
        let t2 = e2.1.as_ref().unwrap();

        if t1 == t2 {
            t1.clone()
        } else if t1 == &Type::Double || t2 == &Type::Double {
            Type::Double
        } else if t1.get_size() == t2.get_size() {
            if t1.is_signed() {
                t2.clone()
            } else {
                t1.clone()
            }
        } else if t1.get_size() > t2.get_size() {
            t1.clone()
        } else {
            t2.clone()
        }
    }

    fn get_common_pointer_type(
        e1: &ExpressionNode,
        e2: &ExpressionNode,
    ) -> Result<Type, Box<dyn Error>> {
        let t1 = e1.1.as_ref().unwrap();
        let t2 = e2.1.as_ref().unwrap();
        if t1 == t2 {
            Ok(t1.clone())
        } else if e1.equals_null_pointer() {
            Ok(t2.clone())
        } else if e2.equals_null_pointer() {
            Ok(t1.clone())
        } else {
            Err(format!(
                "Incompatible types for implicit pointer cast: {:?} and {:?}",
                e1, e2
            )
            .into())
        }
    }

    pub fn equals_null_pointer(&self) -> bool {
        matches!(
            self.0,
            ExpressionWithoutType::Constant(Constant::Integer(0))
                | ExpressionWithoutType::Constant(Constant::Long(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedInteger(0))
                | ExpressionWithoutType::Constant(Constant::UnsignedLong(0))
        )
    }

    pub fn convert_type(&mut self, target: &Type) {
        if self.1.as_ref().unwrap() != target {
            *self = ExpressionNode(
                ExpressionWithoutType::Cast(target.clone(), Box::new(self.clone())),
                Some(target.clone()),
            )
        }
    }

    fn convert_type_by_assignment(&mut self, target: &Type) -> Result<(), Box<dyn Error>> {
        if matches!(target, Type::Array(_, _)) {
            return Err("Can't assign to an array type".into());
        }
        let t1 = self.1.as_ref().unwrap();
        if t1 != target {
            if (t1.is_arithmetic() && target.is_arithmetic())
                || (self.equals_null_pointer() && matches!(target, Type::Pointer(_)))
            {
                self.convert_type(target);
            } else {
                return Err(
                    format!("Can't convert {:?} to {:?} by assignment", t1, target).into(),
                    // format!("Can't convert {:?} to {:?} by assignment", self, target).into(),
                );
            }
        }
        Ok(())
    }

    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if self.1.is_some() {
            // no need to type check this expression a second time
            return Ok(());
        }
        self.1 = Some(match self.0 {
            ExpressionWithoutType::FunctionCall(ref name, ref mut args) => {
                let type_info = context
                    .symbols
                    .get(name)
                    .expect("Function should have been defined")
                    .clone();
                if let Type::Function(out, params) = &type_info.symbol_type {
                    if params.len() != args.len() {
                        return Err("Function call has the wrong number of arguments".into());
                    }
                    for (arg, param) in args.iter_mut().zip(params.iter()) {
                        arg.check_types_and_convert(context)?;
                        arg.convert_type_by_assignment(param)?;
                    }

                    *out.clone()
                } else {
                    return Err("Variable has been called as a function".into());
                }
            }
            ExpressionWithoutType::Var(ref name) => {
                let type_info = context
                    .symbols
                    .get(name)
                    .expect("Var should have been defined");

                match &type_info.symbol_type {
                    Type::Function(_, _) => {
                        return Err("Function has been defined as a variable".into())
                    }
                    t => t.clone(),
                }
            }
            ExpressionWithoutType::Constant(ref c) => match c {
                Constant::Integer(_) => Type::Integer,
                Constant::Long(_) => Type::Long,
                Constant::UnsignedInteger(_) => Type::UnsignedInteger,
                Constant::UnsignedLong(_) => Type::UnsignedLong,
                Constant::Double(_) => Type::Double,
                Constant::Char(_) => Type::Char,
                Constant::UnsignedChar(_) => Type::UnsignedChar,
            },
            ExpressionWithoutType::Unary(ref op, ref mut src) => {
                src.check_types_and_convert(context)?;
                if matches!(
                    op,
                    UnaryOperatorNode::Negate | UnaryOperatorNode::Complement
                ) {
                    if matches!(src.1.as_ref().unwrap(), Type::Pointer(_)) {
                        return Err("Can't apply - or ~ to a pointer".into());
                    }
                    // promote char types to int for this operation
                    if src.1.as_ref().unwrap().is_character() {
                        src.convert_type(&Type::Integer)
                    }
                }
                match op {
                    UnaryOperatorNode::Not => Type::Integer, // returns 0 or 1 (eg boolean-like)
                    UnaryOperatorNode::Complement => {
                        if src.1.as_ref().unwrap() == &Type::Double {
                            return Err("'~' cannot operate on a double".into());
                        }
                        src.1.clone().unwrap()
                    }
                    _ => src.1.clone().unwrap(),
                }
            }
            ExpressionWithoutType::Compound(ref op, ref mut left, ref mut right) => {
                left.check_types_and_convert(context)?;
                right.check_types_and_convert(context)?;
                let mut left_clone = left.clone();
                left_clone.promote(context)?;
                right.promote(context)?;
                // undo type conversion on Left, it will be done manually in the birds generation
                // stage
                let common_type = ExpressionNode::check_types_binary(op, &mut left_clone, right)?;
                // verify that it is possible to assign from the common type to left, without
                // actually doing the conversion yet.
                left_clone.convert_type_by_assignment(&common_type)?;
                common_type
            }
            ExpressionWithoutType::Binary(ref op, ref mut left, ref mut right) => {
                left.check_types_and_convert(context)?;
                right.check_types_and_convert(context)?;
                left.promote(context)?;
                right.promote(context)?;
                ExpressionNode::check_types_binary(op, left, right)?
            }
            ExpressionWithoutType::Assignment(ref mut dst, ref mut src) => {
                dst.check_types_and_convert(context)?;
                src.check_types_and_convert(context)?;
                src.convert_type_by_assignment(dst.1.as_ref().unwrap())?;
                dst.1.clone().unwrap()
            }
            ExpressionWithoutType::Ternary(ref mut cond, ref mut then, ref mut other) => {
                cond.check_types_and_convert(context)?;
                then.check_types_and_convert(context)?;
                other.check_types_and_convert(context)?;
                let common_type = if matches!(then.1.as_ref().unwrap(), Type::Pointer(_))
                    || matches!(other.1.as_ref().unwrap(), Type::Pointer(_))
                {
                    ExpressionNode::get_common_pointer_type(then, other)?
                } else {
                    ExpressionNode::get_common_type(then, other)
                };
                then.convert_type(&common_type);
                other.convert_type(&common_type);

                common_type
            }
            ExpressionWithoutType::Cast(ref target, ref mut e) => {
                e.check_types_and_convert(context)?;
                let src_type = e.1.as_ref().unwrap();
                if matches!((src_type, target), (Type::Double, Type::Pointer(_)))
                    || matches!((target, src_type), (Type::Double, Type::Pointer(_)))
                {
                    return Err("Cannot cast between Double and Pointer types".into());
                }

                if matches!(target, Type::Array(_, _)) {
                    return Err("Cannot cast to an Array type".into());
                }
                target.clone()
            }
            ExpressionWithoutType::Dereference(ref mut e) => {
                e.check_types_and_convert(context)?;
                if let Type::Pointer(t) = e.1.as_ref().unwrap() {
                    *t.clone()
                } else {
                    return Err("Dereference can only operate on pointer types".into());
                }
            }
            ExpressionWithoutType::AddressOf(ref mut e) => {
                // don't convert e to a pointer to the first element here, we want the full array
                e.check_types(context)?;
                if !e.0.match_lvalue() {
                    return Err("Can only take address of an object".into());
                }
                Type::Pointer(Box::new(e.1.clone().unwrap()))
            }
            ExpressionWithoutType::Subscript(ref mut src, ref mut inner) => {
                src.check_types_and_convert(context)?;
                inner.check_types_and_convert(context)?;
                let src_type = src.1.as_ref().unwrap();
                let inner_type = inner.1.as_ref().unwrap();

                if let Type::Pointer(left) = src_type {
                    if !inner_type.is_integer() {
                        return Err("Invalid subscript expression".into());
                    }
                    inner.convert_type(&Type::Long);
                    *left.clone()
                } else if let Type::Pointer(right) = inner_type {
                    if !src_type.is_integer() {
                        return Err("Invalid subscript expression".into());
                    }
                    src.convert_type(&Type::Long);
                    *right.clone()
                } else {
                    return Err("Invalid subscript expression".into());
                }
            }
            ExpressionWithoutType::String(ref s) => {
                // WEE WOO WEE WOO DON'T FORGET TO ASSIGN SPACE FOR THE NULL TERMINATOR
                Type::Array(Box::new(Type::Char), (s.len() as u64) + 1)
            }
        });

        Ok(())
    }

    fn check_types_and_convert(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        self.check_types(context)?;
        self.post_type_check_convert(context)?;

        Ok(())
    }

    fn post_type_check_convert(
        &mut self,
        _context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        // Extra expression inserted here to convert arrays to pointers
        if let Some(Type::Array(t, _)) = &self.1 {
            *self = ExpressionNode(
                ExpressionWithoutType::AddressOf(Box::new(self.clone())),
                Some(Type::Pointer(t.clone())),
            )
        }
        Ok(())
    }

    // promote char and unsigned char to signed int for all unary, binary and switch expressions
    fn promote(&mut self, _context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let t = self.1.as_ref().unwrap().promote().clone();
        self.convert_type(&t);

        Ok(())
    }

    fn check_types_binary(
        op: &BinaryOperatorNode,
        left: &mut ExpressionNode,
        right: &mut ExpressionNode,
    ) -> Result<Type, Box<dyn Error>> {
        let left_type = left.1.as_ref().unwrap();
        let right_type = right.1.as_ref().unwrap();

        // Get the common type of this expression
        // also handle ALL pointer arithmetic type-checking cases in this block
        let (common_type, is_pointer_arithmetic) =
            if (matches!(left_type, Type::Pointer(_)) || matches!(right_type, Type::Pointer(_))) {
                match op {
                    BinaryOperatorNode::Add => {
                        if matches!(left_type, Type::Pointer(_)) && right_type.is_integer() {
                            right.convert_type(&Type::Long);
                            (left_type.clone(), true)
                        } else if matches!(right_type, Type::Pointer(_)) && left_type.is_integer() {
                            left.convert_type(&Type::Long);
                            (right_type.clone(), true)
                        } else {
                            return Err("Invalid pointer addition".into());
                        }
                    }
                    BinaryOperatorNode::Subtract => {
                        if matches!(left_type, Type::Pointer(_)) && right_type.is_integer() {
                            right.convert_type(&Type::Long);
                            (left_type.clone(), true)
                        } else if left_type == right_type {
                            // typedef stuff: for <stddef.h>, this type should be annotated as ptrdiff_t
                            (Type::Long, true)
                        } else {
                            return Err("Invalid pointer subtraction".into());
                        }
                    }
                    BinaryOperatorNode::Less
                    | BinaryOperatorNode::Greater
                    | BinaryOperatorNode::LessEqual
                    | BinaryOperatorNode::GreaterEqual => {
                        if left_type == right_type {
                            (left_type.clone(), false)
                        } else {
                            return Err("Can't compare pointers with different types".into());
                        }
                    }
                    BinaryOperatorNode::Equal | BinaryOperatorNode::NotEqual => {
                        (ExpressionNode::get_common_pointer_type(left, right)?, false)
                    }
                    BinaryOperatorNode::And | BinaryOperatorNode::Or => (Type::Integer, false),
                    o => return Err(format!("Invalid pointer operation: {:?}", o).into()),
                }
            } else {
                (ExpressionNode::get_common_type(left, right), false)
            };

        // convert the left and right to the common type (with exceptions)
        if !is_pointer_arithmetic
            && !matches!(
                op,
                BinaryOperatorNode::ShiftLeft
                    | BinaryOperatorNode::ShiftRight
                    | BinaryOperatorNode::And
                    | BinaryOperatorNode::Or
            )
        {
            left.convert_type(&common_type);
            right.convert_type(&common_type);
        }

        // Misc. Validation of types and operators
        if (matches!(left.1.as_ref().unwrap(), Type::Double)
            && matches!(
                op,
                BinaryOperatorNode::BitwiseAnd
                    | BinaryOperatorNode::BitwiseXor
                    | BinaryOperatorNode::BitwiseOr
                    | BinaryOperatorNode::ShiftLeft
                    | BinaryOperatorNode::ShiftRight
                    | BinaryOperatorNode::Mod
            ))
            || (matches!(right.1.as_ref().unwrap(), Type::Double)
                && matches!(
                    op,
                    BinaryOperatorNode::Mod
                        | BinaryOperatorNode::ShiftLeft
                        | BinaryOperatorNode::ShiftRight
                ))
        {
            return Err("Incompatible types for this binary operation".into());
        }

        Ok(match op {
            BinaryOperatorNode::Add
            | BinaryOperatorNode::Subtract
            | BinaryOperatorNode::Multiply
            | BinaryOperatorNode::Divide
            | BinaryOperatorNode::BitwiseAnd
            | BinaryOperatorNode::BitwiseXor
            | BinaryOperatorNode::BitwiseOr => common_type,
            // types which don't convert the right type (TODO: why is Mod here?)
            BinaryOperatorNode::ShiftLeft
            | BinaryOperatorNode::ShiftRight
            | BinaryOperatorNode::Mod => left.1.clone().unwrap(),
            // logical
            BinaryOperatorNode::And
            | BinaryOperatorNode::Or
            | BinaryOperatorNode::Equal
            | BinaryOperatorNode::NotEqual
            | BinaryOperatorNode::Less
            | BinaryOperatorNode::Greater
            | BinaryOperatorNode::LessEqual
            | BinaryOperatorNode::GreaterEqual => Type::Integer,
        })
    }
}
