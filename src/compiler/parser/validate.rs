use std::{collections::HashMap, error::Error};

use conv::{ApproxInto, ConvUtil, RoundToZero, Wrapping};

use super::{
    BinaryOperatorNode, Block, BlockItemNode, Constant, DeclarationNode, ExpressionNode,
    ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration, InitialValue, OrdinalStatic,
    ProgramNode, StatementNode, StaticInitial, StorageClass, StorageInfo, SwitchMapKey, SymbolInfo,
    Type, UnaryOperatorNode, Validate, ValidateContext, ValidationPass, VariableDeclaration,
};

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
        let this_type = &self.function_type;

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
        let arg_types = match this_type {
            Type::Function(_, v) => v,
            _ => return Err("Incorrect Type for function".into()),
        };

        // add the parameters to the types map if this function has a body. This is done
        // because it allows those parameters to be type checked in the scope that they are
        // actually used (ie, params names from declarations which don't have bodies are thrown
        // out either way).
        // NOTE: PARAMS ARE ADDED TO THE SYMBOL MAP WITH TYPES HERE (this took ages to confirm)
        if self.body.is_some() {
            for (param, arg) in self.params.iter().zip(arg_types.iter()) {
                context.symbols.insert(
                    param.clone(),
                    SymbolInfo {
                        symbol_type: arg.clone(),
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

impl VariableDeclaration {
    fn validate_file_scope(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        // println!("{:?}", self);
        let mut initial_value =
            if let Some(ExpressionWithoutType::Constant(c)) = self.init.as_ref().map(|i| &i.0) {
                InitialValue::Initial(c.convert_to(&self.variable_type))
            } else if self.init.is_none() {
                if matches!(self.storage_class, Some(StorageClass::Extern)) {
                    InitialValue::None
                } else {
                    InitialValue::Tentative
                }
            } else {
                return Err("Initialiser for file-scope variable must be constant".into());
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
                let initial_value = match self.init.as_ref().map(|i| &i.0) {
                    Some(ExpressionWithoutType::Constant(ref c)) => {
                        InitialValue::Initial(c.convert_to(&self.variable_type))
                    }
                    None => match self.variable_type {
                        Type::Integer => InitialValue::Initial(StaticInitial::integer(0)),
                        Type::Long => InitialValue::Initial(StaticInitial::long(0)),
                        Type::UnsignedInteger => {
                            InitialValue::Initial(StaticInitial::unsigned_integer(0))
                        }
                        Type::UnsignedLong => {
                            InitialValue::Initial(StaticInitial::unsigned_long(0))
                        }
                        Type::Double => InitialValue::Initial(StaticInitial::Double(0.)),
                        Type::Function(_, _) => unreachable!(),
                    },
                    _ => {
                        return Err(
                            "Non-constant initialiser on block-scope static variable".into()
                        );
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
                // ensure that this section runs after the variable exists in the symbol table, so
                // that the definition is allowed to refer to its own value
                if let Some(ref mut init) = self.init {
                    init.check_types(context)?;
                    ExpressionNode::convert_type(init, &self.variable_type);
                }
            }
        }
        Ok(())
    }
}

// trait Castable: NumCast + ::Sized {}
// this block will likely move to a type-conversion module of some kind in part 3 for compile-time
// constant-folding
impl Constant {
    pub fn convert_to(&self, target: &Type) -> StaticInitial {
        match self {
            Constant::Double(_) => Self::convert_double_to(self, target),
            _ => Self::convert_ordinal_to(self, target),
        }
    }
    pub fn convert_ordinal_to(&self, target: &Type) -> StaticInitial {
        // match target {}
        match self {
            Constant::Integer(i) => StaticInitial::from_number(*i, target),
            Constant::Long(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitial::from_number(*i, target),
            Constant::Double(_) => unreachable!(), // StaticInitial::from_number(*i, target),
        }
    }
    pub fn convert_double_to(&self, target: &Type) -> StaticInitial {
        // match target {}
        match self {
            Constant::Integer(i) => StaticInitial::from_number(*i, target),
            Constant::Long(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedInteger(i) => StaticInitial::from_number(*i, target),
            Constant::UnsignedLong(i) => StaticInitial::from_number(*i, target),
            Constant::Double(i) => StaticInitial::from_double(*i, target),
        }
    }
}

trait ApproximableOrdinal:
    ApproxInto<i32, Wrapping>
    + ApproxInto<i64, Wrapping>
    + ApproxInto<u32, Wrapping>
    + ApproxInto<u64, Wrapping>
    + ApproxInto<f64>
{
}
impl ApproximableOrdinal for i32 {}
impl ApproximableOrdinal for i64 {}
impl ApproximableOrdinal for u32 {}
impl ApproximableOrdinal for u64 {}

impl StaticInitial {
    fn from_double(i: f64, target: &Type) -> StaticInitial {
        match target {
            Type::Integer => StaticInitial::integer_from_double(i),
            Type::Long => StaticInitial::long_from_double(i),
            Type::UnsignedInteger => StaticInitial::unsigned_integer_from_double(i),
            Type::UnsignedLong => StaticInitial::unsigned_long_from_double(i),
            Type::Double => StaticInitial::Double(i),
            Type::Function(_, _) => unreachable!(),
        }
    }

    fn from_number<T: ApproximableOrdinal>(i: T, target: &Type) -> StaticInitial {
        match target {
            Type::Integer => StaticInitial::integer(i),
            Type::Long => StaticInitial::long(i),
            Type::UnsignedInteger => StaticInitial::unsigned_integer(i),
            Type::UnsignedLong => StaticInitial::unsigned_long(i),
            Type::Double => StaticInitial::double(i),
            Type::Function(_, _) => unreachable!(),
        }
    }

    fn integer<T: ApproxInto<i32, Wrapping>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::Integer(ConvUtil::approx_as_by(i).unwrap()))
    }
    fn long<T: ApproxInto<i64, Wrapping>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::Long(ConvUtil::approx_as_by(i).unwrap()))
    }
    fn unsigned_integer<T: ApproxInto<u32, Wrapping>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::UnsignedInteger(
            ConvUtil::approx_as_by(i).unwrap(),
        ))
    }
    fn unsigned_long<T: ApproxInto<u64, Wrapping>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(
            ConvUtil::approx_as_by(i).unwrap(),
        ))
    }

    fn double<T: ApproxInto<f64>>(i: T) -> StaticInitial {
        StaticInitial::Double(ConvUtil::approx_as(i).unwrap())
    }

    fn integer_from_double<T: ApproxInto<i32, RoundToZero>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::Integer(ConvUtil::approx_as_by(i).unwrap()))
    }
    fn long_from_double<T: ApproxInto<i64, RoundToZero>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::Long(ConvUtil::approx_as_by(i).unwrap()))
    }
    fn unsigned_integer_from_double<T: ApproxInto<u32, RoundToZero>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::UnsignedInteger(
            ConvUtil::approx_as_by(i).unwrap(),
        ))
    }
    fn unsigned_long_from_double<T: ApproxInto<u64, RoundToZero>>(i: T) -> StaticInitial {
        StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(
            ConvUtil::approx_as_by(i).unwrap(),
        ))
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
                        expression.check_types(context)?;
                        let previous =
                            StatementNode::enter_switch_type_checking(expression, context);

                        expression.validate(context)?;
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
                e.check_types(context)?;
                if let Type::Function(out, _) = &context
                    .symbols
                    .get(context.current_function_name.as_ref().unwrap())
                    .unwrap()
                    .symbol_type
                {
                    ExpressionNode::convert_type(e, out);
                } else {
                    unreachable!();
                };
            }
            StatementNode::Case(ref mut e, _, ref label) => {
                e.check_types(context)?;

                let target_type = context
                    .current_switch_type
                    .clone()
                    .ok_or::<Box<dyn Error>>("Case is not in a switch statement".into())?;

                let constant_value = if let ExpressionWithoutType::Constant(ref mut c) = e.0 {
                    c.convert_ordinal_to(&target_type)
                } else {
                    return Err("Non constant expression found in case statement".into());
                };
                let ordinal = if let StaticInitial::Ordinal(ref o) = constant_value {
                    o
                } else {
                    return Err(
                        "None ordinal (eg Double) expression found in case statement".into(),
                    );
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
        context.current_switch_type = e.1.clone();

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
            self.validate_lvalues_are_variables(context)?;
        }
        if matches!(context.pass, ValidationPass::TypeChecking) {
            self.check_types(context)?;
        }

        match self.0 {
            ExpressionWithoutType::Constant(_) => {}
            ExpressionWithoutType::Unary(_, ref mut src) => {
                src.validate(context)?;
            }
            ExpressionWithoutType::Binary(_, ref mut left, ref mut right) => {
                left.validate(context)?;
                right.validate(context)?;
            }
            ExpressionWithoutType::Var(_) => {}
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
        };
        Ok(())
    }
}

impl ExpressionNode {
    fn validate_lvalues_are_variables(
        &mut self,
        _context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
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
                if !src.0.is_lvalue() {
                    return Err(format!(
                        "Can't perform increment/decrement operation on non-variable: {:?}",
                        src,
                    )
                    .into());
                }
            }
            ExpressionWithoutType::Assignment(ref dst, _) => {
                if !dst.0.is_lvalue() {
                    return Err(format!("Can't assign to non-variable: {:?}", dst).into());
                }
            }
            _ => {}
        }
        Ok(())
    }

    // See System V ABI list of rules for how to reconcile types of various sizes
    // C Spec 6.3.1.8, Paragraph 1
    fn get_common_type(t1: &Type, t2: &Type) -> Type {
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

    fn convert_type(src: &mut ExpressionNode, target: &Type) {
        if src.1.clone().unwrap() != *target {
            *src = ExpressionNode(
                ExpressionWithoutType::Cast(target.clone(), Box::new(src.clone())),
                Some(target.clone()),
            )
        }
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
                        arg.check_types(context)?;
                        ExpressionNode::convert_type(arg, param)
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
            },
            ExpressionWithoutType::Unary(ref op, ref mut src) => {
                // replace src in-place in the expression (oh wait i can probably do this
                // everywhere) (yeah)
                src.check_types(context)?;
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
            ExpressionWithoutType::Binary(ref op, ref mut left, ref mut right) => {
                left.check_types(context)?;
                right.check_types(context)?;
                if matches!(op, BinaryOperatorNode::Or | BinaryOperatorNode::And) {
                    Type::Integer // returns 0 or 1 (eg boolean-like)
                } else {
                    let common_type = ExpressionNode::get_common_type(
                        left.1.as_ref().unwrap(),
                        right.1.as_ref().unwrap(),
                    );
                    if !matches!(
                        op,
                        BinaryOperatorNode::ShiftLeft | BinaryOperatorNode::ShiftRight
                    ) {
                        ExpressionNode::convert_type(left, &common_type);
                        ExpressionNode::convert_type(right, &common_type);
                    }

                    match op {
                        BinaryOperatorNode::Add
                        | BinaryOperatorNode::Subtract
                        | BinaryOperatorNode::Multiply
                        | BinaryOperatorNode::Divide => common_type,
                        BinaryOperatorNode::BitwiseOr
                        | BinaryOperatorNode::BitwiseAnd
                        | BinaryOperatorNode::BitwiseXor => {
                            if left.1.as_ref().unwrap() == &Type::Double {
                                return Err("Bitwise operations cannot operate on a double".into());
                            }
                            common_type
                        }
                        BinaryOperatorNode::ShiftLeft
                        | BinaryOperatorNode::ShiftRight
                        | BinaryOperatorNode::Mod => {
                            if left.1.as_ref().unwrap() == &Type::Double {
                                return Err("Bitwise operations cannot operate on a double".into());
                            }
                            if right.1.as_ref().unwrap() == &Type::Double
                                && op != &BinaryOperatorNode::Mod
                            {
                                return Err("Bitwise operations cannot operate on a double".into());
                            }
                            left.1.clone().unwrap()
                        }
                        BinaryOperatorNode::And
                        | BinaryOperatorNode::Or
                        | BinaryOperatorNode::Equal
                        | BinaryOperatorNode::NotEqual
                        | BinaryOperatorNode::Less
                        | BinaryOperatorNode::Greater
                        | BinaryOperatorNode::LessEqual
                        | BinaryOperatorNode::GreaterEqual => Type::Integer,
                    }
                }
            }
            ExpressionWithoutType::Assignment(ref mut dst, ref mut src) => {
                dst.check_types(context)?;
                src.check_types(context)?;
                ExpressionNode::convert_type(src, dst.1.as_ref().unwrap());
                dst.1.clone().unwrap()
            }
            ExpressionWithoutType::Ternary(ref mut cond, ref mut then, ref mut other) => {
                cond.check_types(context)?;
                then.check_types(context)?;
                other.check_types(context)?;
                let common_type = ExpressionNode::get_common_type(
                    then.1.as_ref().unwrap(),
                    other.1.as_ref().unwrap(),
                );
                ExpressionNode::convert_type(then, &common_type);
                ExpressionNode::convert_type(other, &common_type);

                common_type
            }
            ExpressionWithoutType::Cast(ref target, ref mut e) => {
                e.check_types(context)?;
                target.clone()
            }
        });
        Ok(())
    }
}
