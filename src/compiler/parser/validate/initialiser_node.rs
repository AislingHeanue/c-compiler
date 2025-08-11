use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    parser::{ExpressionNode, ExpressionWithoutType, InitialiserNode, InitialiserWithoutType},
    types::{ComparableStatic, Constant, StaticInitialiser, StorageInfo, SymbolInfo, Type},
};

use super::{Validate, ValidateContext};

impl Validate for InitialiserNode {
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self.0 {
            InitialiserWithoutType::Single(ref mut e) => {
                e.validate(context)?;
            }
            InitialiserWithoutType::Compound(ref mut initialisers) => {
                for i in initialisers {
                    i.validate(context)?;
                }
            }
        }
        Ok(())
    }
}

impl InitialiserNode {
    pub fn create_static_init_list(
        &mut self,
        target_type: &Type,
        context: &mut ValidateContext,
    ) -> Result<Vec<StaticInitialiser>, Box<dyn Error>> {
        let init_list: Vec<StaticInitialiser> = match (target_type, &mut self.0) {
            (Type::Pointer(t), InitialiserWithoutType::Single(init_e))
                if init_e.is_string_literal() =>
            {
                if **t != Type::Char {
                    return Err("Can't initialise a non-character pointer with a string".into());
                }
                if let ExpressionWithoutType::String(s) = &init_e.0 {
                    context.num_strings += 1;
                    let new_name = format!("string.constant.{}", context.num_strings);
                    context.symbols.insert(
                        new_name.clone(),
                        SymbolInfo {
                            symbol_type: Type::Array(t.clone(), s.len() as u64 + 1),
                            storage: StorageInfo::Constant(StaticInitialiser::Ordinal(
                                ComparableStatic::String(s.to_vec(), true),
                            )),
                        },
                    );
                    vec![StaticInitialiser::Ordinal(ComparableStatic::Pointer(
                        new_name,
                    ))]
                } else {
                    unreachable!()
                }
            }

            (Type::Array(t, size), InitialiserWithoutType::Single(init_e)) => {
                if let ExpressionWithoutType::String(s) = &init_e.0 {
                    if s.len() > (*size).try_into().unwrap() {
                        return Err("Static String is too long".into());
                    }
                    let difference: i32 = *size as i32 - s.len() as i32;

                    if !t.is_character() {
                        return Err("Can't initialise a non-character array with a string".into());
                    }
                    let mut out = vec![StaticInitialiser::Ordinal(ComparableStatic::String(
                        s.clone(),
                        difference > 0,
                    ))];
                    if difference > 1 {
                        out.push(StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                            difference - 1,
                        )))
                    }
                    out
                } else {
                    return Err("Cannot initialise a static array with a scalar type".into());
                }
            }
            (_, InitialiserWithoutType::Single(ref i)) => {
                if matches!(target_type, Type::Pointer(_)) && !i.equals_null_pointer() {
                    return Err("Cannot initialise a static pointer with a non-pointer type".into());
                }
                let c = if let ExpressionWithoutType::Constant(c) = &i.0 {
                    c
                } else {
                    return Err(
                        "Static variables must be initialised with a constant expression".into(),
                    );
                };

                vec![c.convert_to(target_type)]
            }
            (Type::Array(t, size), InitialiserWithoutType::Compound(ref mut initialisers)) => {
                if initialisers.len() > (*size).try_into().unwrap() {
                    return Err("Too many initialisers in static declaration".into());
                }
                let mut statics: Vec<StaticInitialiser> = process_results(
                    initialisers
                        .iter_mut()
                        .map(|init| init.create_static_init_list(t, context)),
                    |iter| iter.flatten().collect(),
                )?;
                if initialisers.len() < (*size).try_into().unwrap() {
                    let offset = *size as i32 - initialisers.len() as i32;
                    statics.push(StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(
                        t.get_size() * offset,
                    )));
                }
                statics
            }
            (_, InitialiserWithoutType::Compound(_)) => {
                return Err("Only arrays can be initialised with a compound initialiser".into())
            }
        };
        Ok(init_list)
    }

    pub fn check_types(
        &mut self,
        target_type: &Type,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        self.1 = Some(target_type.clone());
        match (target_type, &mut self.0) {
            (Type::Array(t, size), InitialiserWithoutType::Single(ref mut init_e))
                if matches!(&init_e.0, ExpressionWithoutType::String(_s)) =>
            {
                if let ExpressionWithoutType::String(s) = &init_e.0 {
                    if !t.is_character() {
                        return Err(
                            "Can't initialise non-character array with a string literal".into()
                        );
                    }
                    if s.len() > (*size).try_into().unwrap() {
                        return Err("Initialiser has the wrong number of elements".into());
                    }
                } else {
                    unreachable!()
                }
            }

            (_, InitialiserWithoutType::Single(ref mut init_e)) => {
                init_e.check_types_and_convert(context)?;
                init_e.convert_type_by_assignment(target_type)?;
            }
            (Type::Array(t, size), InitialiserWithoutType::Compound(ref mut c_init)) => {
                if c_init.len() > (*size).try_into().unwrap() {
                    return Err("Initialiser has the wrong number of elements".into());
                }
                for init in c_init.iter_mut() {
                    init.check_types(t, context)?;
                }
                while c_init.len() < (*size).try_into().unwrap() {
                    c_init.push(InitialiserNode::zero(t));
                }
            }
            (_, InitialiserWithoutType::Compound(_)) => {
                return Err("Compound initialiser can only be used for arrays".into());
            }
        }
        Ok(())
    }

    pub fn zero(target_type: &Type) -> InitialiserNode {
        match target_type {
            Type::Integer => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Integer(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Long => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Long(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedInteger => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedInteger(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedLong => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Double => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Double(0.)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Array(t, size) => InitialiserNode(
                InitialiserWithoutType::Compound(
                    (0..*size).map(|_| InitialiserNode::zero(t)).collect(),
                ),
                Some(target_type.clone()),
            ),
            Type::Function(_, _) => unreachable!(),
            Type::Pointer(_) => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Char => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Char(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::SignedChar => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Char(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedChar => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedChar(0)),
                    Some(target_type.clone()),
                )),
                Some(target_type.clone()),
            ),
            Type::Void => {
                unreachable!()
            }
        }
    }
}
