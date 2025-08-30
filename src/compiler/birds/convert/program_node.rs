use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    birds::{BirdsProgramNode, BirdsTopLevel},
    parser::{DeclarationNode, ProgramNode},
    types::{Constant, InitialValue, StorageInfo},
};

use super::{Convert, ConvertContext};

impl Convert<BirdsProgramNode> for ProgramNode {
    fn convert(self, context: &mut ConvertContext) -> Result<BirdsProgramNode, Box<dyn Error>> {
        let mut body: Vec<BirdsTopLevel> = process_results(
            self.declarations
                .into_iter()
                .filter_map(|declaration| match declaration {
                    DeclarationNode::Variable(_) => None,
                    DeclarationNode::Type(_) => None,
                    DeclarationNode::Function(f) => Some(f.convert(context)),
                    DeclarationNode::Struct(_) => None,
                    DeclarationNode::Enum(_) => None,
                }),
            |iter| iter.flatten().collect(),
        )?;
        body.append(
            &mut context
                .symbols
                .iter_mut()
                .filter_map(|(name, info)| match &info.storage {
                    StorageInfo::Static(init_value, global) => {
                        let initial = match init_value {
                            InitialValue::Tentative => {
                                vec![Constant::zero(&info.symbol_type, &mut context.structs)]
                            }
                            InitialValue::Initial(i) => i.clone(),
                            InitialValue::None => return None,
                        };
                        Some(BirdsTopLevel::StaticVariable(
                            info.symbol_type.clone(),
                            name.clone(),
                            initial,
                            *global,
                        ))
                    }
                    StorageInfo::Constant(init) => Some(BirdsTopLevel::StaticConstant(
                        info.symbol_type.clone(),
                        name.clone(),
                        init.clone(),
                    )),
                    _ => None,
                })
                .collect(),
        );
        Ok(BirdsProgramNode { body })
    }
}
