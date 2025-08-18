use crate::compiler::{
    birds::{BirdsProgramNode, BirdsValueNode},
    parser::StructInfo,
    types::{Class, StaticInitialiser, SymbolInfo, Type},
};
use itertools::{process_results, Itertools};
use std::{collections::HashMap, error::Error};

use super::{
    AssemblyType, BinaryOperator, ConditionCode, ImmediateValue, Instruction, Operand, Program,
    Register, TopLevel, UnaryOperator, DOUBLE_PARAM_REGISTERS, FUNCTION_PARAM_REGISTERS,
};

mod assembly_type;
mod instruction;
mod operand;
mod operators;
mod top_level;

pub trait Convert<T>
where
    Self: Sized,
{
    fn convert(self, context: &mut ConvertContext) -> Result<T, Box<dyn Error>>;
}

pub struct ConvertContext {
    pub symbols: HashMap<String, SymbolInfo>,
    pub structs: HashMap<String, StructInfo>,
    pub struct_classes: HashMap<String, Vec<Class>>,
    pub comments: bool,
    pub is_mac: bool,
    pub is_linux: bool,
    constants: HashMap<String, (u32, StaticInitialiser)>,
    pub num_labels: u32,
}

impl ConvertContext {
    pub fn new(
        comments: bool,
        linux: bool,
        mac: bool,
        symbols: HashMap<String, SymbolInfo>,
        structs: HashMap<String, StructInfo>,
    ) -> ConvertContext {
        ConvertContext {
            comments,
            is_linux: linux,
            is_mac: mac,
            symbols,
            structs,
            constants: HashMap::new(),
            struct_classes: HashMap::new(),
            num_labels: 0,
        }
    }
}

struct Args {
    double: Vec<Operand>,
    integer: Vec<(Operand, bool, AssemblyType)>,
    stack: Vec<(Operand, bool, AssemblyType)>,
}

#[derive(Debug)]
struct ReturnInfo {
    integer: Vec<(Operand, AssemblyType)>,
    double: Vec<Operand>,
    uses_memory: bool,
}

fn classify_function_args(
    values: Vec<BirdsValueNode>,
    return_value_uses_memory: bool,
    context: &mut ConvertContext,
) -> Result<Args, Box<dyn Error>> {
    let mut args = Args {
        integer: Vec::new(),
        double: Vec::new(),
        stack: Vec::new(),
    };

    let integer_reg_offset = if return_value_uses_memory { 1 } else { 0 };

    for v in values {
        let (t, is_signed, is_scalar) = AssemblyType::infer(&v, context)?;
        if t == AssemblyType::Double {
            if args.double.len() < 8 {
                args.double.push(v.convert(context)?);
            } else {
                args.stack.push((v.convert(context)?, is_signed, t));
            }
        } else if is_scalar {
            if args.integer.len() + integer_reg_offset < 6 {
                args.integer.push((v.convert(context)?, is_signed, t));
            } else {
                args.stack.push((v.convert(context)?, is_signed, t));
            }
        } else {
            // oh god v is a struct everyone panic
            // NOTE: v cannot be a function because that's super illegal, and can't be an array
            // because it would have decayed to a pointer in all cases. It also can't be void
            // because that's also crimes.
            let var_name = get_var_name(&v);
            let struct_type = context.symbols.get(&var_name).unwrap().symbol_type.clone();
            let classes = classify_struct(&struct_type, context);

            let mut throw_it_onto_the_stack = true;
            let size = t.get_size();

            if *classes.first().unwrap() != Class::Memory {
                let mut maybe_ints = Vec::new();
                let mut maybe_doubles = Vec::new();
                for i in 0_i32..classes.len() as i32 {
                    if *classes.get(i as usize).unwrap() == Class::Sse {
                        maybe_doubles.push(Operand::MockMemory(var_name.clone(), i * 8))
                    } else {
                        maybe_ints.push((
                            Operand::MockMemory(var_name.clone(), i * 8),
                            false,
                            AssemblyType::get_eightbyte(i * 8, size as i32),
                        ));
                    }
                }
                if maybe_doubles.len() + args.double.len() <= 8
                    && maybe_ints.len() + integer_reg_offset + args.integer.len() <= 6
                {
                    args.double.append(&mut maybe_doubles);
                    args.integer.append(&mut maybe_ints);
                    throw_it_onto_the_stack = false;
                }
            }
            if throw_it_onto_the_stack {
                for i in 0_i32..classes.len() as i32 {
                    args.stack.push((
                        Operand::MockMemory(var_name.clone(), i * 8),
                        false,
                        AssemblyType::get_eightbyte(i * 8, size as i32),
                    ));
                }
            }
        }
    }

    Ok(args)
}

fn classify_return(
    v: BirdsValueNode,
    context: &mut ConvertContext,
) -> Result<ReturnInfo, Box<dyn Error>> {
    let (t, _, is_scalar) = AssemblyType::infer(&v, context)?;
    let mut out = ReturnInfo {
        integer: Vec::new(),
        double: Vec::new(),
        uses_memory: false,
    };
    if t == AssemblyType::Double {
        out.double.push(v.convert(context)?);
    } else if is_scalar {
        out.integer.push((v.convert(context)?, t));
    } else {
        let var_name = get_var_name(&v);
        let struct_type = context.symbols.get(&var_name).unwrap().symbol_type.clone();

        let classes = classify_struct(&struct_type, context);
        let size = t.get_size();

        if *classes.first().unwrap() == Class::Memory {
            out.uses_memory = true;
        } else {
            for i in 0_i32..classes.len() as i32 {
                match classes.get(i as usize).unwrap() {
                    Class::Sse => {
                        out.double
                            .push(Operand::MockMemory(var_name.clone(), i * 8));
                    }
                    Class::Integer => {
                        out.integer.push((
                            Operand::MockMemory(var_name.clone(), i * 8),
                            AssemblyType::get_eightbyte(i * 8, size as i32),
                        ));
                    }
                    Class::Memory => unreachable!(),
                }
            }
        }
    }
    Ok(out)
}

pub fn classify_return_type(
    t: &Type,
    context: &mut ConvertContext,
) -> Result<bool, Box<dyn Error>> {
    if *t == Type::Void {
        Ok(false)
    } else {
        let is_scalar = t.is_scalar();
        if !t.is_complete(&mut context.structs) {
            // otherwise, this function causes declarations of functions with incomplete return
            // types to misbehave, even if those functions are never used.
            return Ok(false);
        }
        let assembly_type = t.clone().convert(context)?;

        if assembly_type == AssemblyType::Double || is_scalar {
            Ok(false)
        } else if let Type::Struct(_, _) = t {
            let classes = classify_struct(t, context);

            Ok(*classes.first().unwrap() == Class::Memory)
        } else {
            // return type already can't be arrays or functions
            unreachable!()
        }
    }
}

fn get_var_name(v: &BirdsValueNode) -> String {
    if let BirdsValueNode::Var(var_name) = v {
        var_name.to_string()
    } else {
        unreachable!()
    }
}

fn classify_struct(struct_type: &Type, context: &mut ConvertContext) -> Vec<Class> {
    let (name, _is_union) = if let Type::Struct(name, is_union) = struct_type {
        (name, is_union)
    } else {
        unreachable!()
    };
    if let Some(classes) = context.struct_classes.get(name) {
        return classes.to_vec();
    }
    let info = context.structs.get(name).unwrap().clone();
    let out = if info.size > 16 {
        vec![Class::Memory; u64::div_ceil(info.size, 8) as usize]
    } else {
        let member_types = flatten_struct_types(struct_type, context);
        // size > 8 means there are exactly 2 eightbytes we need to classify
        // (this would probably be more complicated if we supported floats)
        if info.size > 8 {
            match (
                member_types.first().unwrap().class(),
                member_types.last().unwrap().class(),
            ) {
                (Class::Sse, Class::Sse) => vec![Class::Sse, Class::Sse],
                (Class::Sse, _) => vec![Class::Sse, Class::Integer],
                (_, Class::Sse) => vec![Class::Integer, Class::Sse],

                (_, _) => vec![Class::Integer, Class::Integer],
            }
        } else {
            match member_types.first().unwrap().class() {
                Class::Sse => vec![Class::Sse],
                _ => vec![Class::Integer],
            }
        }
    };
    context.struct_classes.insert(name.to_string(), out.clone());
    out
}

fn flatten_struct_types(this_type: &Type, context: &mut ConvertContext) -> Vec<Type> {
    match this_type {
        Type::Struct(name, false) => {
            let info = context.structs.get(name).unwrap().clone();
            info.members
                .iter()
                .flat_map(|m| flatten_struct_types(&m.member_type, context))
                .collect()
        }
        Type::Struct(name, true) => {
            // this whole block is a hack and should probably be moved to the parent function,
            // but for now that would involve rewriting some code I don't want to touch, at least
            // until/if I add more types eg. float
            let info = context.structs.get(name).unwrap().clone();
            let mut type_possibilities = Vec::new();
            for member in info.members {
                type_possibilities.push(flatten_struct_types(&member.member_type, context));
            }
            if info.size <= 8 {
                let mut out_type = Type::Double; // so that we get [Class::Sse]
                for possibility in type_possibilities {
                    if possibility.iter().any(|t| t.class() != Class::Sse) {
                        out_type = Type::Long; // so that we get [Class::Integer]
                    }
                }
                vec![out_type] // so that the resulting class is [Integer]
            } else {
                // we can now assume we need to return 2 classes exactly since if size > 16 then we
                // write to memory automatically
                let mut first_out_type = Type::Double;
                let mut second_out_type = Type::Double;
                for possibility in type_possibilities {
                    if possibility.first().unwrap().class() != Class::Sse {
                        first_out_type = Type::Long;
                    }
                    if possibility
                        .iter()
                        .map(|t| t.get_size(&mut context.structs))
                        .sum::<u64>()
                        > 8
                        && possibility.last().unwrap().class() != Class::Sse
                    {
                        second_out_type = Type::Long;
                    }
                }
                vec![first_out_type, second_out_type]
            }
        }
        Type::Array(inner_type, size) => {
            let out_types = flatten_struct_types(inner_type, context);
            // if we encounter an array of size N, then flatten the inner type and
            // duplicate it N times
            vec![out_types; *size as usize]
                .into_iter()
                .flatten()
                .collect()
        }
        _ => vec![this_type.clone()],
    }
}

fn create_static_constant(
    alignment: u32,
    value: StaticInitialiser,
    context: &mut ConvertContext,
) -> Operand {
    if let Some(key) = context
        .constants
        .iter()
        .find(|(_, v)| **v == (alignment, value.clone()))
        .map(|(k, _)| k)
    {
        return Operand::MockReg(key.clone());
    }

    let new_name = format!("constant_{}", context.constants.len() + 1);
    context
        .constants
        .insert(new_name.clone(), (alignment, value));

    Operand::MockReg(new_name)
}

impl<U, V> Convert<Vec<U>> for Vec<V>
where
    V: Convert<U>,
{
    fn convert(self, context: &mut ConvertContext) -> Result<Vec<U>, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|function| function.convert(context)),
            |iter| iter.collect(),
        )
    }
}

impl Convert<Program> for BirdsProgramNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Program, Box<dyn Error>> {
        let mut body = self.body.convert(context)?;

        for (name, (align, value)) in context
            .constants
            .iter()
            .sorted_by(|(a, _), (b, _)| a.cmp(b))
        {
            body.push(TopLevel::StaticConstant(
                name.clone(),
                *align,
                value.clone(),
            ))
        }

        Ok(Program {
            body,
            displaying_context: None,
        })
    }
}
