use std::{collections::HashMap, error::Error};

use crate::compiler::{
    parser::{ExpressionNode, ExpressionWithoutType, StatementNode, SwitchMapKey},
    types::{StaticInitialiser, Type},
};

use super::{CheckTypes, Validate, ValidateContext, ValidationPass};

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
            StatementNode::Expression(ref mut e) => {
                e.validate(context)?;
                if matches!(context.pass, ValidationPass::TypeChecking)
                    && !e.1.clone().unwrap().is_complete(&mut context.structs)
                    && e.1.clone().unwrap() != Type::Void
                {
                    return Err("An expression statement cannot return an incomplete type".into());
                }
            }
            StatementNode::Pass => {}
            StatementNode::Return(ref mut e) => e.validate(context)?,
            StatementNode::If(ref mut condition, ref mut then, ref mut otherwise) => {
                condition.validate(context)?;
                if matches!(context.pass, ValidationPass::TypeChecking) {
                    condition.check_types(context)?;
                    if !condition.1.as_ref().unwrap().is_scalar() {
                        return Err(
                            "Can't construct an if statement with a non-scalar condition".into(),
                        );
                    }
                }
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
                    if matches!(context.pass, ValidationPass::TypeChecking) {
                        expression.check_types(context)?;
                        if !expression.1.as_ref().unwrap().is_scalar() {
                            return Err(
                                "Can't construct a while loop with a non-scalar condition".into()
                            );
                        }
                    }
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
                    if matches!(context.pass, ValidationPass::TypeChecking) {
                        expression.check_types(context)?;
                        if !expression.1.as_ref().unwrap().is_scalar() {
                            return Err(
                                "Can't construct a do-while loop with a non-scalar condition"
                                    .into(),
                            );
                        }
                    }
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
                    if matches!(context.pass, ValidationPass::TypeChecking) {
                        init.check_types(context)?;
                        if cond.is_some() {
                            let cond = cond.as_mut().unwrap();
                            cond.check_types(context)?;
                            if !cond.1.as_ref().unwrap().is_scalar() {
                                return Err(
                                    "Can't construct a for loop with a non-scalar condition".into(),
                                );
                            }
                        }
                    }
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

impl CheckTypes for StatementNode {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            StatementNode::Return(exp) => {
                let out_type = if let Type::Function(out, _) = &context
                    .symbols
                    .get(context.current_function_name.as_ref().unwrap())
                    .unwrap()
                    .symbol_type
                {
                    out.clone()
                } else {
                    unreachable!();
                };

                if let Some(e) = exp {
                    e.check_types_and_convert(context)?;
                    e.convert_type_by_assignment(&out_type, context)?;
                } else if *out_type != Type::Void {
                    return Err("Return in non-void function must have a value".into());
                }
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
                let ordinal = if let StaticInitialiser::Comparable(ref o) = constant_value {
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
