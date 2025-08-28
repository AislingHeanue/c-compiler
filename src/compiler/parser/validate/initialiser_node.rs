use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    parser::{ExpressionNode, ExpressionWithoutType, InitialiserNode, InitialiserWithoutType},
    types::{
        ComparableStatic, Constant, Count, Flatten, StaticInitialiser, StorageInfo, SymbolInfo,
        Type,
    },
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
        let init_list: Vec<StaticInitialiser> = match (target_type, self.0.clone()) {
            (Type::Pointer(t, _), InitialiserWithoutType::Single(init_e))
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
                            storage: StorageInfo::Constant(StaticInitialiser::Comparable(
                                ComparableStatic::String(s.to_vec(), true),
                            )),
                            constant: false,
                            volatile: false,
                        },
                    );
                    vec![StaticInitialiser::Comparable(ComparableStatic::Pointer(
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
                    let mut out = vec![StaticInitialiser::Comparable(ComparableStatic::String(
                        s.clone(),
                        difference > 0,
                    ))];
                    if difference > 1 {
                        out.push(StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                            (difference - 1).try_into().unwrap(),
                        )))
                    }
                    out
                } else {
                    return Err("Cannot initialise a static array with a scalar type".into());
                }
            }
            (Type::Struct(_, _), InitialiserWithoutType::Single(_)) => {
                return Err(
                    "Structs and unions must be initialised with a compound expression".into(),
                );
            }
            (_, InitialiserWithoutType::Single(ref i)) => {
                if matches!(target_type, Type::Pointer(_, _)) && !i.equals_null_pointer() {
                    return Err("Cannot initialise a static pointer with a non-pointer type".into());
                }
                let c = if let ExpressionWithoutType::Constant(c) = &i.0 {
                    c
                } else {
                    return Err(
                        "Static variables must be initialised with a constant expression".into(),
                    );
                };

                vec![c.static_convert_to(target_type)]
            }
            (Type::Array(t, size), InitialiserWithoutType::Compound(ref mut initialisers)) => {
                let expected_count = (*size).try_into().unwrap();
                if initialisers.len() > expected_count {
                    return Err(format!(
                        "Too many initialisers in static declaration of array, got {}, expected {}",
                        initialisers.len(),
                        expected_count
                    )
                    .into());
                }
                let mut statics: Vec<StaticInitialiser> = process_results(
                    initialisers
                        .iter_mut()
                        .map(|init| init.create_static_init_list(t, context)),
                    |iter| iter.flatten().collect(),
                )?;
                if initialisers.len() < (*size).try_into().unwrap() {
                    let offset = *size - initialisers.len() as u64;
                    statics.push(StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                        t.get_size(&mut context.structs) * offset,
                    )));
                }
                statics
            }
            (
                Type::Struct(name, is_union),
                InitialiserWithoutType::Compound(ref mut initialisers),
            ) => {
                let info = context
                    .structs
                    .get(name)
                    .ok_or::<Box<dyn Error>>("Can't initialise an incomplete struct".into())?
                    .clone();

                if *is_union {
                    if initialisers.len() > 1 {
                        return Err("Too many initialisers in static declaration of union".into());
                    }
                } else if initialisers.len() > info.members.count(*is_union, &mut context.structs) {
                    return Err("Too many initialisers in static declaration of struct".into());
                }

                let mut offset = 0_u64;
                let mut statics = Vec::new();
                for (init, (member, member_offset)) in initialisers
                    .iter_mut()
                    .zip(info.members.flatten(*is_union, &mut context.structs))
                {
                    if member_offset as u64 != offset {
                        statics.push(StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                            member_offset as u64 - offset,
                        )));
                    }
                    statics
                        .append(&mut init.create_static_init_list(&member.member_type, context)?);
                    offset =
                        member_offset as u64 + member.member_type.get_size(&mut context.structs);
                }

                if info.size > offset {
                    statics.push(StaticInitialiser::Comparable(ComparableStatic::ZeroBytes(
                        info.size - offset,
                    )));
                }
                statics
            }
            (_, InitialiserWithoutType::Compound(_)) => {
                return Err(
                    "Only arrays and structs can be initialised with a compound initialiser".into(),
                )
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
                init_e.convert_type_by_assignment(target_type, context)?;
            }
            (Type::Array(t, size), InitialiserWithoutType::Compound(ref mut c_init)) => {
                if c_init.len() > (*size).try_into().unwrap() {
                    return Err("Initialiser has the wrong number of elements".into());
                }
                for init in c_init.iter_mut() {
                    init.check_types(t, context)?;
                }
                while c_init.len() < (*size).try_into().unwrap() {
                    c_init.push(InitialiserNode::zero(t, context));
                }
            }
            (Type::Struct(name, is_union), InitialiserWithoutType::Compound(ref mut c_init)) => {
                let info = context
                    .structs
                    .get(name)
                    .ok_or::<Box<dyn Error>>("Can't initialise an incomplete struct".into())?
                    .clone();

                let expected_count = info.members.count(*is_union, &mut context.structs);
                if c_init.len() > expected_count {
                    return Err(format!(
                        "Too many initialisers in declaration of struct, got {}, expected {}",
                        c_init.len(),
                        expected_count
                    )
                    .into());
                }

                for (init, (member, _)) in c_init
                    .iter_mut()
                    .zip(info.members.flatten(*is_union, &mut context.structs))
                {
                    init.check_types(&member.member_type, context)?;
                }
                for i in c_init.clone().len()..info.members.count(*is_union, &mut context.structs) {
                    c_init.push(InitialiserNode::zero(
                        &info.members.get(i).unwrap().member_type,
                        context,
                    ));
                }
            }
            (_t, InitialiserWithoutType::Compound(_)) => {
                return Err("Compound initialiser can only be used for arrays and structs".into());
            }
        }
        Ok(())
    }

    pub fn zero(target_type: &Type, context: &mut ValidateContext) -> InitialiserNode {
        match target_type {
            Type::Integer => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Integer(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Long => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Long(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::LongLong => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::LongLong(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedInteger => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedInteger(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedLong => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedLongLong => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLongLong(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Float => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Float(0.)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Double => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Double(0.)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::LongDouble => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::LongDouble(0.)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Array(t, size) => InitialiserNode(
                InitialiserWithoutType::Compound(
                    (0..*size)
                        .map(|_| InitialiserNode::zero(t, context))
                        .collect(),
                ),
                Some(target_type.clone()),
            ),
            Type::Function(_, _) => unreachable!(),
            Type::Pointer(_, _) => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedLong(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Char => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Char(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::SignedChar => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Char(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedChar => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedChar(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Short => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::Short(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::UnsignedShort => InitialiserNode(
                InitialiserWithoutType::Single(ExpressionNode(
                    ExpressionWithoutType::Constant(Constant::UnsignedShort(0)),
                    Some(target_type.clone()),
                    false,
                )),
                Some(target_type.clone()),
            ),
            Type::Struct(name, is_union) => {
                let mut members = context.structs.get(name).unwrap().members.clone();
                // only zero the first value in a union
                if *is_union {
                    members = vec![members.first().unwrap().clone()];
                }

                let mut c_init = Vec::new();
                for m in members {
                    c_init.push(InitialiserNode::zero(&m.member_type, context));
                }
                InitialiserNode(
                    InitialiserWithoutType::Compound(c_init),
                    Some(target_type.clone()),
                )
            }
            Type::Void => {
                unreachable!()
            }
        }
    }
}
