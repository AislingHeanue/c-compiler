use itertools::process_results;
use std::error::Error;

use crate::compiler::IndentDisplay;

use super::{
    Block, BlockItemNode, DeclarationNode, ExpressionNode, ProgramNode, StatementNode, Validate,
    ValidateContext,
};

impl Validate for ProgramNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        self.function.body = self.function.body.validate(context)?;
        Ok(self)
    }
}

impl Validate for Block {
    fn validate(self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|i| match i {
                BlockItemNode::Declaration(d) => {
                    let new_d = d.validate(context);
                    if let Ok(newer_d) = new_d {
                        Ok(BlockItemNode::Declaration(newer_d))
                    } else {
                        Err(new_d.err().unwrap())
                    }
                }
                BlockItemNode::Statement(s) => {
                    let new_s = s.validate(context);
                    if let Ok(newer_s) = new_s {
                        Ok(BlockItemNode::Statement(newer_s))
                    } else {
                        Err(new_s.err().unwrap())
                    }
                }
            }),
            |iter| iter.collect(),
        )
    }
}

impl Validate for DeclarationNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            DeclarationNode::Declaration(t, name, expression) => {
                if context.variables.contains_key(&name) {
                    return Err(format!("Duplicate definition of name: {}", name).into());
                }
                context.num_variables += 1;
                let new_name = format!("{}:{}", name, context.num_variables);
                context.variables.insert(name.to_string(), new_name.clone());
                self = if let Some(e) = expression {
                    DeclarationNode::Declaration(
                        t,
                        new_name.to_string(),
                        Some(e.validate(context)?),
                    )
                } else {
                    DeclarationNode::Declaration(t, new_name.to_string(), None)
                }
            }
        }
        Ok(self)
    }
}

impl Validate for StatementNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
        match self {
            StatementNode::Expression(e) => {
                self = StatementNode::Expression(e.validate(context)?);
            }
            StatementNode::Pass => {}
            StatementNode::Return(e) => {
                self = StatementNode::Return(e.validate(context)?);
            }
        }
        Ok(self)
    }
}

impl Validate for ExpressionNode {
    fn validate(mut self, context: &mut ValidateContext) -> Result<Self, Box<dyn Error>> {
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
            ExpressionNode::Var(ref name) => {
                if !context.variables.contains_key(name) {
                    return Err(format!("Variable used before declaration: {}", name).into());
                } else {
                    context.num_variables += 1;
                    let new_name = format!("{}:{}", name, context.num_variables);
                    context.variables.insert(name.to_string(), new_name.clone());
                    self = ExpressionNode::Var(new_name)
                }
            }
            ExpressionNode::Assignment(dst, src) => {
                if !matches!(*dst, ExpressionNode::Var(_)) {
                    return Err(format!("Found a non-variable to the left side of an assignment, can't assign to {:?}", dst.fmt_indent(0, false)).into());
                }
                self = ExpressionNode::Assignment(
                    Box::new(dst.validate(context)?),
                    Box::new(src.validate(context)?),
                )
            }
        }
        Ok(self)
    }
}
