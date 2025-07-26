use itertools::process_results;
use std::{collections::HashMap, error::Error};

use super::{
    Block, BlockItemNode, DeclarationNode, ExpressionNode, ForInitialiserNode, FunctionDeclaration,
    ProgramNode, StatementNode, UnaryOperatorNode, Validate, ValidateContext, ValidationPass,
    VariableDeclaration,
};

impl Validate for ProgramNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        self.functions = process_results(
            self.functions.into_iter().map(|f| f.validate(context)),
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
        context.current_function_name = Some(self.name.clone());
        self.body = self.body.validate(context)?;
        Ok(self)
    }
}

impl Validate for Option<Block> {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if let Some(b) = self {
            self = Some(b.validate(context)?)
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
    fn validate(self, _context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        Ok(self)
    }
}

impl Validate for StatementNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::ReadLabels) {
            self = Self::read_labels(self, context)?;
        }
        if matches!(context.pass, ValidationPass::ValidateLabels) {
            self = Self::validate_labels(self, context)?;
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
            StatementNode::Break(label) => {
                if matches!(context.pass, ValidationPass::LabelLoops) {
                    self = StatementNode::Break(Some(
                        label
                            .or(context.current_enclosing_loop_name.clone())
                            .ok_or::<Box<dyn Error>>("Break is not inside a loop".into())?,
                    ))
                } else {
                    self = StatementNode::Break(label)
                }
            }
            StatementNode::Continue(label) => {
                if matches!(context.pass, ValidationPass::LabelLoops) {
                    self = StatementNode::Continue(Some(
                        label
                            .or(context.current_enclosing_loop_name.clone())
                            .ok_or::<Box<dyn Error>>("Continue is not inside a loop".into())?,
                    ))
                } else {
                    self = StatementNode::Continue(label)
                }
            }
            StatementNode::While(expression, body, label) => {
                let previous_loop_name = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_loop(context)
                } else {
                    None
                };

                self = StatementNode::While(
                    expression.validate(context)?,
                    Box::new(body.validate(context)?),
                    label.or(context.current_enclosing_loop_name.clone()),
                );

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::leave_loop(previous_loop_name, context)
                }
            }
            StatementNode::DoWhile(body, expression, label) => {
                let previous_loop_name = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_loop(context)
                } else {
                    None
                };

                self = StatementNode::DoWhile(
                    Box::new(body.validate(context)?),
                    expression.validate(context)?,
                    label.or(context.current_enclosing_loop_name.clone()),
                );

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::leave_loop(previous_loop_name, context)
                }
            }
            StatementNode::For(init, cond, post, body, label) => {
                let previous_loop_name = if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::enter_loop(context)
                } else {
                    None
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
                    label.or(context.current_enclosing_loop_name.clone()),
                );

                if matches!(context.pass, ValidationPass::LabelLoops) {
                    StatementNode::leave_loop(previous_loop_name, context)
                }
            }
            StatementNode::Switch(expression, body, label) => {
                self = StatementNode::Switch(
                    expression.validate(context)?,
                    Box::new(body.validate(context)?),
                    label,
                )
            }
            StatementNode::Case(expression, body) => {
                self = StatementNode::Case(
                    expression.validate(context)?,
                    Box::new(body.validate(context)?),
                )
            }
            StatementNode::Default(body) => {
                self = StatementNode::Default(Box::new(body.validate(context)?))
            }
        }
        Ok(self)
    }
}

impl Validate for ForInitialiserNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(v) => {
                self = ForInitialiserNode::Declaration(VariableDeclaration {
                    variable_type: v.variable_type,
                    name: v.name,
                    init: v.init.validate(context)?,
                })
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

    fn enter_loop(context: &mut ValidateContext) -> Option<String> {
        context.num_loops += 1;
        let new_loops_name = format!("loop_{}", context.num_loops);

        let previous_loop_name = context.current_enclosing_loop_name.clone();
        context.current_enclosing_loop_name = Some(new_loops_name);

        previous_loop_name
    }

    fn leave_loop(previous_name: Option<String>, context: &mut ValidateContext) {
        context.current_enclosing_loop_name = previous_name;
    }
}

impl Validate for Option<ExpressionNode> {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if let Some(e) = self {
            self = Some(e.validate(context)?)
        }
        Ok(self)
    }
}

impl Validate for ExpressionNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        if matches!(context.pass, ValidationPass::CheckLvalues) {
            self = ExpressionNode::validate_lvalues_are_variables(self, context)?;
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
            ExpressionNode::FunctionCall(_, _) => todo!(),
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
}
