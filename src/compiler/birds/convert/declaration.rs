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

use super::{expression_node::E, Convert, ConvertContext};

impl Convert<Vec<BirdsInstructionNode>> for DeclarationNode {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        match self {
            DeclarationNode::Variable(v) => Ok(v.convert(context)?),
            // function declaration at the top scope does not use this path, since it is read
            // directly in ProgramNode.
            // function declarations without a body do not emit instructions, so this branch is
            // ignored entirely.
            DeclarationNode::Function(_f) => Ok(Vec::new()),
            DeclarationNode::Type(_t) => Ok(Vec::new()),
            DeclarationNode::Struct(_s) => Ok(Vec::new()),
        }
    }
}

impl Convert<Option<BirdsTopLevel>> for FunctionDeclaration {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<Option<BirdsTopLevel>, Box<dyn Error>> {
        let name = self.name;
        let params = self.params.to_vec();
        if let Some(body) = self.body {
            let mut instructions = body.convert(context)?;
            // add an extra "return 0;" at the end because the C standard dictates that if main() exits
            // without a return statement, then it must actually return 0. If a return statement is
            // otherwise present, this instruction will never be run (dead code).
            let map_entry = context.symbols.get(&name).unwrap();
            if let Type::Function(out, _) = &map_entry.symbol_type {
                if **out != Type::Void {
                    instructions.push(BirdsInstructionNode::Return(Some(
                        BirdsValueNode::Constant(Constant::Integer(0)),
                    )));
                } else {
                    instructions.push(BirdsInstructionNode::Return(None));
                }
            } else {
                unreachable!()
            }
            if let StorageInfo::Function(_defined, global) = &map_entry.storage {
                Ok(Some(BirdsTopLevel::Function(
                    name,
                    params,
                    instructions,
                    *global,
                )))
            } else {
                unreachable!()
            }
        } else {
            Ok(None)
        }
    }
}

impl Convert<Vec<BirdsInstructionNode>> for VariableDeclaration {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
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
                    InitialiserNode::pad_bytes(
                        difference_in_size,
                        &mut instructions,
                        dst.clone(),
                        context,
                    );
                    context.current_initialiser_offset += difference_in_size;
                } else {
                    unreachable!()
                }

                Ok(instructions)
            }
            (InitialiserWithoutType::Single(e), _) => {
                let offset = e.1.as_ref().unwrap().get_size(&mut context.structs);
                let (mut instructions, new_src): E = e.convert(context)?;
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
                context.current_initialiser_offset += offset as i32;

                Ok(instructions)
            }
            (InitialiserWithoutType::Compound(initialisers), Type::Struct(name)) => {
                let info = context.structs.get(&name).unwrap().clone();
                let mut instructions = Vec::new();
                let original_offset = context.current_initialiser_offset;
                for (init, member) in initialisers.into_iter().zip(info.members.iter()) {
                    let difference_in_size = member.offset as i32
                        - (context.current_initialiser_offset - original_offset);
                    // optional addition: initialise all the in-between bytes of the struct to zero
                    // println!(
                    //     "Padding {} bytes, member offset = {}, member type = {:?}, name = {}",
                    //     difference_in_size, member.offset, member.member_type, name
                    // );
                    InitialiserNode::pad_bytes(
                        difference_in_size,
                        &mut instructions,
                        dst.clone(),
                        context,
                    );

                    context.current_initialiser_offset = original_offset + member.offset as i32;
                    instructions.append(&mut init.convert(dst.clone(), context)?);
                }
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

    fn pad_bytes(
        num: i32,
        instructions: &mut Vec<BirdsInstructionNode>,
        dst: String,
        context: &mut ConvertContext,
    ) {
        let mut i = 0;
        while i < num {
            if i + 8 <= num {
                instructions.push(BirdsInstructionNode::CopyToOffset(
                    BirdsValueNode::Constant(Constant::get_typed(0, &Type::Long)),
                    dst.clone(),
                    context.current_initialiser_offset + i,
                ));
                i += 8;
            } else if i + 4 <= num {
                instructions.push(BirdsInstructionNode::CopyToOffset(
                    BirdsValueNode::Constant(Constant::get_typed(0, &Type::Integer)),
                    dst.clone(),
                    context.current_initialiser_offset + i,
                ));
                i += 4;
            } else {
                instructions.push(BirdsInstructionNode::CopyToOffset(
                    BirdsValueNode::Constant(Constant::get_typed(0, &Type::Char)),
                    dst.clone(),
                    context.current_initialiser_offset + i,
                ));
                i += 1;
            }
        }
    }
}
