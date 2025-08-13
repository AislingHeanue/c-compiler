use std::{collections::HashMap, error::Error};

mod expression_node;
mod function_declaration;
mod initialiser_node;
mod parsed_types;
mod statement_node;
mod struct_declaration;
mod variable_declaration;

use super::{
    BlockItemNode, DeclarationNode, ForInitialiserNode, ProgramNode, StructInfo, SwitchMapKey,
    SymbolInfo, Type,
};
// initialiser representing n * 0x00 bytes

trait Validate
where
    Self: Sized,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
}

trait CheckTypes
where
    Self: Sized,
{
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
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
pub struct ValidateContext {
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
    structs: HashMap<String, StructInfo>,
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
        structs: HashMap::new(),
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

impl Validate for DeclarationNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            DeclarationNode::Variable(v) => v.validate(context)?,
            DeclarationNode::Function(f) => f.validate(context)?,
            DeclarationNode::Type(t) => {
                if matches!(context.pass, ValidationPass::TypeChecking) {
                    if let Some(ref mut s) = t.struct_declaration {
                        s.check_types(context)?;
                    }
                    t.target_type.check_types(context)?;
                }
            }
            DeclarationNode::Struct(s) => {
                if matches!(context.pass, ValidationPass::TypeChecking) {
                    s.check_types(context)?;
                }
            }
        };
        Ok(())
    }
}

impl Validate for Vec<BlockItemNode> {
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

impl Validate for ForInitialiserNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(v) => v.validate(context)?,
            ForInitialiserNode::Expression(e) => e.validate(context)?,
        }
        Ok(())
    }
}
impl CheckTypes for ForInitialiserNode {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(_v) => {}
            ForInitialiserNode::Expression(None) => {}
            ForInitialiserNode::Expression(Some(e)) => e.check_types_and_convert(context)?,
        }
        Ok(())
    }
}
