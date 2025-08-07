use std::error::Error;

use crate::compiler::{
    birds::{BirdsInstructionNode, BirdsTopLevel, BirdsValueNode},
    parser::{DeclarationNode, FunctionDeclaration, VariableDeclaration},
    types::{Constant, StorageInfo},
};

use super::{Convert, ConvertContext};

impl Convert for DeclarationNode {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            DeclarationNode::Variable(v) => Ok(v.convert(context)?),
            // function declaration at the top scope does not use this path, since it is read
            // directly in ProgramNode.
            // function declarations without a body do not emit instructions, so this branch is
            // ignored entirely.
            DeclarationNode::Function(_f) => Ok(Vec::new()),
        }
    }
}

impl Convert for FunctionDeclaration {
    type Output = Option<BirdsTopLevel>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        let name = self.name;
        let params = self.params.to_vec();
        if let Some(body) = self.body {
            let mut instructions = body.convert(context)?;
            // add an extra "return 0;" at the end because the C standard dictates that if main() exits
            // without a return statement, then it must actually return 0. If a return statement is
            // otherwise present, this instruction will never be run (dead code).
            instructions.push(BirdsInstructionNode::Return(BirdsValueNode::Constant(
                Constant::Integer(0),
            )));
            if let StorageInfo::Function(_defined, global) =
                context.symbols.get(&name).unwrap().storage
            {
                Ok(Some(BirdsTopLevel::Function(
                    name,
                    params,
                    instructions,
                    global,
                )))
            } else {
                panic!("Symbol info missing from map for defined function")
            }
        } else {
            Ok(None)
        }
    }
}

impl Convert for VariableDeclaration {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        // do not emit any instructions for static and extern variable definitions. These are
        // handled after the rest of the ProgramNode has been converted.
        if self.storage_class.is_some() {
            Ok(Vec::new())
        } else if let Some(init) = self.init {
            context.current_initialiser_offset = 0;
            init.convert(self.name, context)
        } else {
            Ok(Vec::new())
        }
    }
}
