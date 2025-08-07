use std::error::Error;

use itertools::{process_results, Itertools};

use crate::compiler::{
    birds::{BirdsInstructionNode, BirdsValueNode},
    parser::{ExpressionWithoutType, InitialiserNode, InitialiserWithoutType},
    types::Type,
};

use super::{get_typed_constant, ConvertContext, ConvertEvaluate};
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
                                get_typed_constant(converted, &Type::Long),
                                dst.clone(),
                                context.current_initialiser_offset,
                            ));

                            context.current_initialiser_offset += 8;
                        } else if count >= 4 {
                            let (one, two, three, four) = iter.next_tuple().unwrap();
                            let bytes = [*one as u8, *two as u8, *three as u8, *four as u8];
                            let converted = i32::from_le_bytes(bytes);
                            instructions.push(BirdsInstructionNode::CopyToOffset(
                                get_typed_constant(converted.into(), &Type::Integer),
                                dst.clone(),
                                context.current_initialiser_offset,
                            ));

                            context.current_initialiser_offset += 4;
                        } else {
                            for next in iter.by_ref() {
                                instructions.push(BirdsInstructionNode::CopyToOffset(
                                    get_typed_constant((*next).into(), &Type::Char),
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
                            get_typed_constant(0, &Type::Char),
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
