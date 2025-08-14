use std::{collections::HashSet, error::Error};

use crate::compiler::{
    codegen::align_stack_size,
    parser::{MemberEntry, StructDeclaration, StructInfo, StructMember},
    types::Type,
};

use super::{CheckTypes, ValidateContext};

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

        if context.structs.contains_key(&self.name) {
            return Err(format!("Struct {} is defined twice in the same scope", self.name).into());
        }

        for m in members.iter_mut() {
            if let Some(ref name) = m.name {
                if context.current_struct_members.contains(name) {
                    return Err("Repeated name in struct definition".into());
                }
                context.current_struct_members.insert(name.clone());
            }

            if let Some(ref mut s) = m.struct_declaration {
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
            let this_offset = align_stack_size(size, this_alignment);
            entries.push(MemberEntry {
                member_type: m.member_type.clone(),
                name: m.name.clone(),
                offset: this_offset,
            });
            alignment = u64::max(alignment, this_alignment);
            size = this_offset + m.member_type.get_size(&mut context.structs);
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
                return Err(format!("Duplicate name in struct: {:?}", name).into());
            }
            context.current_struct_members.insert(name.clone());
        } else if let Type::Struct(ref name) = self.member_type {
            // this is an anonymous struct. Treat all its members as if they're embedded in this
            // struct
            let mut info = context.structs.get(name).unwrap().clone();
            for member in info.members.iter_mut() {
                member.member_type.check_types(context)?;
            }
            context.structs.insert(name.clone(), info);
        } else {
            unreachable!()
        }

        if let Some(ref mut s) = self.struct_declaration {
            // if m is itself a struct definition, bring said definition into scope
            s.check_types(context)?;
        }
        Ok(())
    }
}
