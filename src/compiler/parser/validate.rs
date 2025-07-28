use itertools::process_results;
use std::{collections::HashMap, error::Error};

use super::{
    Block, BlockItemNode, DeclarationNode, ExpressionNode, ForInitialiserNode, FunctionDeclaration,
    InitialValue, ProgramNode, StatementNode, StorageClass, StorageInfo, SwitchMapKey, SymbolInfo,
    Type, UnaryOperatorNode, Validate, ValidateContext, ValidationPass, VariableDeclaration,
};

impl<T> Validate for Option<T>
where
    T: Validate,
{
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if let Some(e) = self {
            self = Some(e.validate(context)?)
        }
        Ok(self)
    }
}

impl<T> Validate for Vec<T>
where
    T: Validate,
{
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        self = process_results(self.into_iter().map(|e| e.validate(context)), |iter| {
            iter.collect()
        })?;
        Ok(self)
    }
}

impl Validate for ProgramNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        self.declarations = process_results(
            self.declarations.into_iter().map(|f| f.validate(context)),
            |iter| iter.collect(),
        )?;
        Ok(self)
    }
}

impl Validate for FunctionDeclaration {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::ReadLabels) {
            context.labels.insert(self.name.clone(), HashMap::new());
        }
        let previous_function_name = context.current_function_name.clone();
        context.current_function_name = Some(self.name.clone());

        if matches!(context.pass, ValidationPass::TypeChecking) {
            let this_type = Type::Function(
                Box::new(self.out_type.clone()),
                self.params.iter().map(|(t, _name)| t.clone()).collect(),
            );

            let mut is_defined = self.body.is_some();
            let mut is_global = !matches!(self.storage_class, Some(StorageClass::Static));

            if let Some(old_symbol_info) = context.symbols.get(&self.name) {
                if this_type != old_symbol_info.symbol_type {
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
                    symbol_type: this_type,
                    storage: StorageInfo::Function(is_defined, is_global),
                },
            );

            // add the parameters to the types map if this function has a body. This is done
            // because it allows those parameters to be type checked in the scope that they are
            // actually used (ie, params names from declarations which don't have bodies are thrown
            // out either way).
            if self.body.is_some() {
                for param in &self.params {
                    context.symbols.insert(
                        param.1.clone(),
                        SymbolInfo {
                            symbol_type: param.0.clone(),
                            storage: StorageInfo::Automatic,
                        },
                    );
                }
            }
        }

        self.body = self.body.validate(context)?;
        // reset current_function_name because it's an easy way to check if we are currently in a
        // block scope or file scope at the moment. May need to revisit this.
        context.current_function_name = previous_function_name;
        Ok(self)
    }
}
impl Validate for VariableDeclaration {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::TypeChecking) {
            if context.current_function_name.is_none() {
                self = self.validate_file_scope(context)?
            } else {
                self = self.validate_block_scope(context)?
            }
        }
        self.init = self.init.validate(context)?;
        Ok(self)
    }
}

impl VariableDeclaration {
    fn validate_file_scope(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        // println!("{:?}", self);
        let mut initial_value = if let Some(ExpressionNode::IntegerConstant(i)) = self.init {
            InitialValue::Initial(i)
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
            if Type::Integer != old_symbol_info.symbol_type {
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
                symbol_type: self.out_type.clone(),
                storage: StorageInfo::Static(initial_value, is_global),
            },
        );
        Ok(self)
    }

    fn validate_block_scope(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self.storage_class {
            Some(StorageClass::Extern) => {
                if self.init.is_some() {
                    return Err("Extern block-scope variable may not have an initialiser".into());
                }
                if let Some(old_symbol_info) = context.symbols.get(&self.name) {
                    if old_symbol_info.symbol_type != self.out_type {
                        return Err("Variable redeclared with Incompatible type".into());
                    }
                } else {
                    context.symbols.insert(
                        self.name.clone(),
                        SymbolInfo {
                            symbol_type: self.out_type.clone(),
                            storage: StorageInfo::Static(InitialValue::None, true),
                        },
                    );
                }
            }
            Some(StorageClass::Static) => {
                let initial_value = match self.init {
                    Some(ExpressionNode::IntegerConstant(i)) => InitialValue::Initial(i),
                    None => InitialValue::Initial(0),
                    _ => {
                        return Err(
                            "Non-constant initialiser on block-scope static variable".into()
                        );
                    }
                };
                context.symbols.insert(
                    self.name.clone(),
                    SymbolInfo {
                        symbol_type: self.out_type.clone(),
                        storage: StorageInfo::Static(initial_value, false),
                    },
                );
            }
            None => {
                context.symbols.insert(
                    self.name.clone(),
                    SymbolInfo {
                        symbol_type: self.out_type.clone(),
                        storage: StorageInfo::Automatic,
                    },
                );
            }
        }
        Ok(self)
    }
}

impl Validate for Block {
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|i| match i {
                BlockItemNode::Declaration(d) => {
                    Ok(BlockItemNode::Declaration(d.validate(context)?))
                }
                BlockItemNode::Statement(s) => Ok(BlockItemNode::Statement(s.validate(context)?)),
            }),
            |iter| iter.collect(),
        )
    }
}

impl Validate for DeclarationNode {
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            DeclarationNode::Variable(v) => Ok(DeclarationNode::Variable(v.validate(context)?)),
            DeclarationNode::Function(f) => Ok(DeclarationNode::Function(f.validate(context)?)),
        }
    }
}

impl Validate for StatementNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match context.pass {
            ValidationPass::ReadLabels => {
                self = Self::read_labels(self, context)?;
            }
            ValidationPass::ValidateLabels => {
                self = Self::validate_labels(self, context)?;
            }
            // LabelLoops covers the context required by
            // all loops, switch, break, continue, case, default
            ValidationPass::LabelLoops => {
                self = Self::label_loops(self, context)?;
            }
            ValidationPass::ConstantCases => {
                self = Self::validate_constant_cases(self, context)?;
            }
            ValidationPass::CheckLvalues => {}
            ValidationPass::TypeChecking => {}
        }
        match self {
            StatementNode::Expression(e) => self = StatementNode::Expression(e.validate(context)?),
            StatementNode::Pass => {}
            StatementNode::Return(e) => self = StatementNode::Return(e.validate(context)?),
            StatementNode::If(condition, then, otherwise) => {
                let new_other = match *otherwise {
                    Some(other) => Some(other.validate(context)?),
                    None => None,
                };
                self = StatementNode::If(
                    condition.validate(context)?,
                    Box::new(then.validate(context)?),
                    Box::new(new_other),
                )
            }
            StatementNode::Label(s, statement) => {
                self = StatementNode::Label(s, Box::new(statement.validate(context)?))
            }
            StatementNode::Goto(_) => {}
            StatementNode::Compound(block) => {
                self = StatementNode::Compound(block.validate(context)?)
            }
            StatementNode::Break(_) => {}
            StatementNode::Continue(_) => {}
            StatementNode::While(expression, body, label) => {
                let previous_loop_names = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_loop(context)
                } else {
                    (None, None)
                };

                self = StatementNode::While(
                    expression.validate(context)?,
                    Box::new(body.validate(context)?),
                    label.or(context.current_enclosing_loop_name_for_break.clone()),
                );

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::leave_loop(previous_loop_names, context)
                }
            }
            StatementNode::DoWhile(body, expression, label) => {
                let previous_loop_name = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_loop(context)
                } else {
                    (None, None)
                };

                self = StatementNode::DoWhile(
                    Box::new(body.validate(context)?),
                    expression.validate(context)?,
                    label.or(context.current_enclosing_loop_name_for_break.clone()),
                );

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::leave_loop(previous_loop_name, context)
                }
            }
            StatementNode::For(init, cond, post, body, label) => {
                let previous_loop_name = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_loop(context)
                } else {
                    (None, None)
                };

                self = StatementNode::For(
                    init.validate(context)?,
                    process_results(cond.map(|cond| cond.validate(context)), |mut iter| {
                        iter.next()
                    })?,
                    process_results(post.map(|post| post.validate(context)), |mut iter| {
                        iter.next()
                    })?,
                    Box::new(body.validate(context)?),
                    label.or(context.current_enclosing_loop_name_for_break.clone()),
                );

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::leave_loop(previous_loop_name, context)
                }
            }
            StatementNode::Switch(expression, body, label, map) => {
                let previous_loop_names = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_switch(context)
                } else {
                    (None, None, None)
                };

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    let body_inner = body.validate(context)?;
                    self = StatementNode::Switch(
                        expression.validate(context)?,
                        Box::new(body_inner),
                        label.or(context.current_enclosing_loop_name_for_break.clone()),
                        context.current_switch_labels.take(),
                    );
                    StatementNode::leave_switch(previous_loop_names, context)
                } else {
                    self = StatementNode::Switch(
                        expression.validate(context)?,
                        Box::new(body.validate(context)?),
                        label.or(context.current_enclosing_loop_name_for_break.clone()),
                        map,
                    );
                }
            }
            StatementNode::Case(expression, body, label) => {
                self = StatementNode::Case(
                    expression.validate(context)?,
                    Box::new(body.validate(context)?),
                    label,
                )
            }
            StatementNode::Default(body, label) => {
                self = StatementNode::Default(Box::new(body.validate(context)?), label)
            }
        }
        Ok(self)
    }
}

impl Validate for ForInitialiserNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(v) => {
                self = ForInitialiserNode::Declaration(v.validate(context)?)
            }
            ForInitialiserNode::Expression(e) => {
                self = ForInitialiserNode::Expression(e.validate(context)?)
            }
        }
        Ok(self)
    }
}

impl StatementNode {
    fn read_labels(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if let StatementNode::Label(s, statement) = self {
            let labels_defined = context
                .labels
                .get_mut(&context.current_function_name.clone().unwrap())
                .unwrap();

            if labels_defined.contains_key(&s) {
                return Err(format!("Duplicate label name definition: {}", s).into());
            }
            context.num_labels += 1;
            let new_name = format!("{}_user_{}", s, context.num_labels);
            labels_defined.insert(s.clone(), new_name.clone());

            self = StatementNode::Label(new_name, statement)
        }
        Ok(self)
    }

    fn validate_labels(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if let StatementNode::Goto(s) = self {
            let labels_defined = context
                .labels
                .get_mut(&context.current_function_name.clone().unwrap())
                .unwrap();
            let new_name = labels_defined
                .get(&s)
                .ok_or(format!("Label not found: {}", s))?;
            self = StatementNode::Goto(new_name.to_string());
        }

        Ok(self)
    }

    fn label_loops(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            StatementNode::Break(label) => {
                self = StatementNode::Break(Some(
                    label
                        .or(context.current_enclosing_loop_name_for_break.clone())
                        .ok_or::<Box<dyn Error>>("Break is not inside a loop or switch".into())?,
                ))
            }
            StatementNode::Continue(label) => {
                self = StatementNode::Continue(Some(
                    label
                        .or(context.current_enclosing_loop_name_for_continue.clone())
                        .ok_or::<Box<dyn Error>>("Continue is not inside a loop".into())?,
                ))
            }
            StatementNode::Case(expression, body, label) => {
                let switch_label = label
                    .or(context.current_enclosing_loop_name_for_case.clone())
                    .ok_or::<Box<dyn Error>>("Case is not inside a switch".into())?;

                context.num_switch_labels += 1;
                let new_case_name = format!("{}_case_{}", switch_label, context.num_switch_labels);

                let already_present = context
                    .current_switch_labels
                    .as_mut()
                    .ok_or("Switch label map not found")?
                    .insert(
                        SwitchMapKey::Expression(expression.clone()),
                        new_case_name.clone(),
                    );

                if already_present.is_some() {
                    return Err(
                        format!("Duplicate case expression in switch: {:?}", expression).into(),
                    );
                }

                self = StatementNode::Case(expression, body, Some(new_case_name))
            }
            StatementNode::Default(body, label) => {
                let switch_label = label
                    .or(context.current_enclosing_loop_name_for_case.clone())
                    .ok_or::<Box<dyn Error>>("Default is not inside a switch".into())?;

                let new_name = format!("{}_default", switch_label);

                let already_present = context
                    .current_switch_labels
                    .as_mut()
                    .ok_or("Switch label map not found")?
                    .insert(SwitchMapKey::Default, new_name.clone());

                if already_present.is_some() {
                    return Err("Duplicate default in switch".into());
                }

                self = StatementNode::Default(body, Some(new_name))
            }
            _ => {}
        };
        Ok(self)
    }

    fn validate_constant_cases(
        mut self,
        _context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        if let StatementNode::Case(expression, body, label) = self {
            if let ExpressionNode::IntegerConstant(i) = expression {
                self = StatementNode::Case(ExpressionNode::IntegerConstant(i), body, label);
                Ok(self)
            } else {
                Err(format!("Case expression must be constant, got {:?}", expression).into())
            }
        } else {
            Ok(self)
        }
    }

    // fn check_types(mut self, _context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
    //     match self {
    //         StatementNode::Expression(_) => todo!(),
    //         StatementNode::Return(_) => todo!(),
    //     }
    // }

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

    fn enter_switch(
        context: &mut ValidateContext,
    ) -> (
        Option<String>,
        Option<String>,
        Option<HashMap<SwitchMapKey, String>>,
    ) {
        context.num_switches += 1;
        let new_switch_name = format!("switch_{}", context.num_switches);

        let previous_loop_name_for_break = context.current_enclosing_loop_name_for_break.clone();
        context.current_enclosing_loop_name_for_break = Some(new_switch_name.clone());

        let previous_loop_name_for_case = context.current_enclosing_loop_name_for_case.clone();
        context.current_enclosing_loop_name_for_case = Some(new_switch_name);

        let previous_switch_labels = context.current_switch_labels.clone();
        context.current_switch_labels = Some(HashMap::new());

        (
            previous_loop_name_for_break,
            previous_loop_name_for_case,
            previous_switch_labels,
        )
    }

    fn leave_loop(previous_names: (Option<String>, Option<String>), context: &mut ValidateContext) {
        context.current_enclosing_loop_name_for_break = previous_names.0;
        context.current_enclosing_loop_name_for_continue = previous_names.1;
    }

    fn leave_switch(
        previous_names: (
            Option<String>,
            Option<String>,
            Option<HashMap<SwitchMapKey, String>>,
        ),
        context: &mut ValidateContext,
    ) {
        context.current_enclosing_loop_name_for_break = previous_names.0;
        context.current_enclosing_loop_name_for_case = previous_names.1;
        context.current_switch_labels = previous_names.2;
    }
}

impl Validate for ExpressionNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::CheckLvalues) {
            self = ExpressionNode::validate_lvalues_are_variables(self, context)?;
        }
        if matches!(context.pass, ValidationPass::TypeChecking) {
            self = ExpressionNode::check_types(self, context)?;
        }

        match self {
            ExpressionNode::IntegerConstant(_) => {}
            ExpressionNode::Unary(op, src) => {
                self = ExpressionNode::Unary(op, Box::new(src.validate(context)?))
            }
            ExpressionNode::Binary(op, left, right) => {
                self = ExpressionNode::Binary(
                    op,
                    Box::new(left.validate(context)?),
                    Box::new(right.validate(context)?),
                )
            }
            ExpressionNode::Var(_) => {}
            ExpressionNode::Assignment(dst, src) => {
                self = ExpressionNode::Assignment(
                    Box::new(dst.validate(context)?),
                    Box::new(src.validate(context)?),
                )
            }
            ExpressionNode::Ternary(condition, then, otherwise) => {
                self = ExpressionNode::Ternary(
                    Box::new(condition.validate(context)?),
                    Box::new(then.validate(context)?),
                    Box::new(otherwise.validate(context)?),
                )
            }
            ExpressionNode::FunctionCall(name, params) => {
                self = ExpressionNode::FunctionCall(name, params.validate(context)?)
            }
        }
        Ok(self)
    }
}

impl ExpressionNode {
    fn validate_lvalues_are_variables(
        mut self,
        _context: &mut ValidateContext,
    ) -> Result<Self, Box<dyn Error>> {
        match self {
            ExpressionNode::Unary(op, src) => {
                // VALIDATION: Make sure ++ and -- only operate on variables, not constants or
                // other expressions
                match op {
                    UnaryOperatorNode::PrefixIncrement
                    | UnaryOperatorNode::PrefixDecrement
                    | UnaryOperatorNode::SuffixIncrement
                    | UnaryOperatorNode::SuffixDecrement => {
                        if !src.is_lvalue() {
                            return Err(format!(
                                "Can't perform increment/decrement operation on non-variable: {:?}",
                                src,
                            )
                            .into());
                        }
                    }
                    _ => {}
                }
                self = ExpressionNode::Unary(op, src)
            }
            ExpressionNode::Assignment(dst, src) => {
                if !dst.is_lvalue() {
                    return Err(format!("Can't assign to non-variable: {:?}", src,).into());
                }
                self = ExpressionNode::Assignment(dst, src)
            }
            _ => {}
        }
        Ok(self)
    }

    fn check_types(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            ExpressionNode::FunctionCall(name, args) => {
                let type_info = context
                    .symbols
                    .get(&name)
                    .expect("Function should have been defined");
                if let Type::Function(_out, params) = &type_info.symbol_type {
                    if params.len() != args.len() {
                        return Err("Function call has the wrong number of arguments".into());
                    }
                    self = ExpressionNode::FunctionCall(name, args.validate(context)?)
                } else {
                    return Err("Variable has been called as a function".into());
                }
            }
            ExpressionNode::Var(name) => {
                let type_info = context
                    .symbols
                    .get(&name)
                    .expect("Var should have been defined");

                if type_info.symbol_type != Type::Integer {
                    return Err("Function has been defined as a variable".into());
                }

                self = ExpressionNode::Var(name)
            }
            _ => {}
        }
        Ok(self)
    }
}
