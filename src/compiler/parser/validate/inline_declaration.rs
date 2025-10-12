use std::{collections::HashSet, error::Error};

use crate::compiler::{
    codegen::align_stack_size,
    parser::{
        EnumDeclaration, InlineDeclaration, MemberEntry, StructDeclaration, StructInfo,
        StructMember,
    },
    types::{ComparableStatic, EnumMember, StaticInitialiser, StorageInfo, SymbolInfo, Type},
};

use super::{CheckTypes, ValidateContext};

impl CheckTypes for InlineDeclaration {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        match self {
            InlineDeclaration::Struct(s) => s.check_types(context),
            InlineDeclaration::Enum(e) => e.check_types(context),
        }
    }
}

impl CheckTypes for StructDeclaration {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let original_seen_list = context.current_struct_members.clone();
        if !context.checking_embedded_struct {
            context.current_struct_members = HashSet::new();
        }

        let members = if let Some(ref mut members) = self.members {
            members
        } else {
            return Ok(());
        };
        // println!("STRUCT MEMBERS {:#?}", members);

        if context.structs.contains_key(&self.name) {
            return Err(format!(
                "Struct or union {} is defined twice in the same scope",
                self.name
            )
            .into());
        }

        for m in members.iter_mut() {
            if let Some(ref name) = m.name {
                if context.current_struct_members.contains(name) {
                    return Err("Repeated name in struct or union definition".into());
                }
                context.current_struct_members.insert(name.clone());
            }

            for s in m.inline_declarations.iter_mut() {
                let original_checking_embedded_struct = context.checking_embedded_struct;
                if m.name.is_none() {
                    context.checking_embedded_struct = true;
                }

                // if m is itself a struct definition, bring said definition into scope
                s.check_types(context)?;

                if m.name.is_none() {
                    context.checking_embedded_struct = original_checking_embedded_struct;
                }
            }

            m.member_type.check_types(context)?;

            // this also prevents recursive structs, since structs that have not been fully
            // resolved into the structs map are not complete
            if !m.member_type.is_complete(&mut context.structs) {
                return Err("Structs may only be composed of complete types".into());
            }
        }

        let mut entries = Vec::new();
        let mut size = 0;
        let mut alignment = 1;
        for m in members.iter_mut() {
            let this_alignment = m.member_type.get_alignment(&mut context.structs);

            let this_offset = if self.is_union {
                // union entries are all at offset zero.
                0
            } else {
                // struct entries are sequential (with padding if needed)
                align_stack_size(size, this_alignment)
            };
            if m.name.is_none() && !matches!(m.member_type, Type::Enum(_) | Type::Struct(_, _)) {
                return Err("Anonymous struct member must be one of enum, struct or union".into());
            }

            entries.push(MemberEntry {
                member_type: m.member_type.clone(),
                name: m.name.clone(),
                offset: this_offset,
            });

            alignment = u64::max(alignment, this_alignment);

            size = if self.is_union {
                // sizeof union is the size of its largest member (aligned)
                u64::max(size, m.member_type.get_size(&mut context.structs))
            } else {
                // sizeof struct is the sum of its member sizes and padding (aligned)
                this_offset + m.member_type.get_size(&mut context.structs)
            }
        }

        size = align_stack_size(size, alignment);

        context.structs.insert(
            self.name.clone(),
            StructInfo {
                alignment,
                size,
                members: entries,
            },
        );

        if !context.checking_embedded_struct {
            context.current_struct_members = original_seen_list;
        }

        Ok(())
    }
}

impl CheckTypes for StructMember {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        if let Some(ref name) = self.name {
            if context.current_struct_members.contains(name) {
                return Err(format!("Duplicate name in struct or union: {:?}", name).into());
            }
            context.current_struct_members.insert(name.clone());
        } else if let Type::Struct(ref name, _) = self.member_type {
            // this is an anonymous struct. Type check it and make it a member of this struct like
            // any other member. Field resolution is handled by Vec<MemberEntry>::flatten() etc.
            let mut info = context.structs.get(name).unwrap().clone();
            for member in info.members.iter_mut() {
                member.member_type.check_types(context)?;
            }
            context.structs.insert(name.clone(), info);
        } else {
            unreachable!()
        }

        for s in self.inline_declarations.iter_mut() {
            // if m is itself a struct definition, bring said definition into scope
            s.check_types(context)?;
        }
        Ok(())
    }
}

impl CheckTypes for EnumDeclaration {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        for m in self.members.iter_mut() {
            m.check_types(context)?;
        }
        Ok(())
    }
}

impl CheckTypes for EnumMember {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        context.symbols.insert(
            // if an enum was specified as part of an enum declaration, then it was given an
            // internal name for this purpose
            self.internal_name.clone().unwrap(),
            SymbolInfo {
                symbol_type: Type::Integer,
                storage: StorageInfo::Constant(StaticInitialiser::Comparable(if self.init == 0 {
                    ComparableStatic::ZeroBytes(4)
                } else {
                    ComparableStatic::Integer(self.init)
                })),
                constant: true,
                volatile: false,
            },
        );
        Ok(())
    }
}
