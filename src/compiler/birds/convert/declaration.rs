use std::error::Error;

use crate::compiler::{
    birds::{BirdsInstructionNode, BirdsTopLevel, BirdsValueNode},
    parser::{DeclarationNode, FunctionDeclaration, VariableDeclaration},
    types::{Constant, StorageInfo},
};
use itertools::{process_results, Itertools};

use crate::compiler::{
    parser::{ExpressionWithoutType, InitialiserNode, InitialiserWithoutType},
    types::Type,
};

use super::ConvertEvaluate;

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

impl InitialiserNode {
    pub fn convert(
        self,
        dst: String,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        match (self.0, self.1.unwrap()) {
            (InitialiserWithoutType::Single(e), Type::Array(t, size))
                if e.is_string_literal() && t.is_character() =>
            {
                let mut instructions = Vec::new();
                let first_offset = context.current_initialiser_offset;
                if let ExpressionWithoutType::String(v) = e.0 {
                    let mut iter = v.iter().peekable();
                    while iter.peek().is_some() {
                        let count = iter.clone().count();
                        // if we can take 8 values at the same time, do so and convert them to a
                        // long
                        if count >= 8 {
                            let (one, two, three, four, five, six, seven, eight) =
                                iter.next_tuple().unwrap();
                            let bytes = [
                                *one as u8,
                                *two as u8,
                                *three as u8,
                                *four as u8,
                                *five as u8,
                                *six as u8,
                                *seven as u8,
                                *eight as u8,
                            ];
                            let converted = i64::from_le_bytes(bytes);
                            instructions.push(BirdsInstructionNode::CopyToOffset(
                                BirdsValueNode::Constant(Constant::get_typed(
                                    converted,
                                    &Type::Long,
                                )),
                                dst.clone(),
                                context.current_initialiser_offset,
                            ));

                            context.current_initialiser_offset += 8;
                        } else if count >= 4 {
                            let (one, two, three, four) = iter.next_tuple().unwrap();
                            let bytes = [*one as u8, *two as u8, *three as u8, *four as u8];
                            let converted = i32::from_le_bytes(bytes);
                            instructions.push(BirdsInstructionNode::CopyToOffset(
                                BirdsValueNode::Constant(Constant::get_typed(
                                    converted.into(),
                                    &Type::Integer,
                                )),
                                dst.clone(),
                                context.current_initialiser_offset,
                            ));

                            context.current_initialiser_offset += 4;
                        } else {
                            for next in iter.by_ref() {
                                instructions.push(BirdsInstructionNode::CopyToOffset(
                                    BirdsValueNode::Constant(Constant::get_typed(
                                        (*next).into(),
                                        &Type::Char,
                                    )),
                                    dst.clone(),
                                    context.current_initialiser_offset,
                                ));
                                context.current_initialiser_offset += 1;
                            }
                        }
                    }
                    // for every space left in the array, add null bytes
                    let difference_in_size =
                        size as i32 - (context.current_initialiser_offset - first_offset);
                    for _ in 0..difference_in_size {
                        println!("{:?} {}", instructions, difference_in_size);
                        instructions.push(BirdsInstructionNode::CopyToOffset(
                            BirdsValueNode::Constant(Constant::get_typed(0, &Type::Char)),
                            dst.clone(),
                            context.current_initialiser_offset,
                        ));
                        context.current_initialiser_offset += 1;
                    }
                } else {
                    unreachable!()
                }

                Ok(instructions)
            }
            (InitialiserWithoutType::Single(e), _) => {
                let offset = e.1.as_ref().unwrap().get_size();
                let (mut instructions, new_src) = e.convert_and_evaluate(context)?;
                if context.current_initialiser_offset == 0 {
                    instructions.push(BirdsInstructionNode::Copy(
                        new_src,
                        BirdsValueNode::Var(dst),
                    ));
                } else {
                    instructions.push(BirdsInstructionNode::CopyToOffset(
                        new_src,
                        dst,
                        context.current_initialiser_offset,
                    ));
                }
                context.current_initialiser_offset += offset;

                Ok(instructions)
            }
            (InitialiserWithoutType::Compound(initialisers), _) => {
                let instructions: Vec<BirdsInstructionNode> = process_results(
                    initialisers
                        .into_iter()
                        .map(|init| init.convert(dst.clone(), context)),
                    |iter| iter.flatten().collect(),
                )?;
                Ok(instructions)
            }
        }
    }
}
