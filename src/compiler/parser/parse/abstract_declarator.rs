// use itertools::process_results;
//
// use super::{AbstractDeclarator, Parse, ParseContext, Type};
// use crate::compiler::{
//     lexer::{Token, TokenVector},
//     parser::{ParamDeclarator, StructDeclaration},
// };
// use std::{collections::VecDeque, error::Error};
//
// impl Parse<(AbstractDeclarator, Vec<StructDeclaration>)> for VecDeque<Token> {
//     fn parse(
//         &mut self,
//         context: &mut ParseContext,
//     ) -> Result<(AbstractDeclarator, Vec<StructDeclaration>), Box<dyn Error>> {
//         match self.peek()? {
//             Token::Star => {
//                 self.expect(Token::Star)?;
//                 match self.peek()? {
//                     Token::KeywordRestrict => {
//                         self.expect(Token::KeywordRestrict)?;
//                         let inner: (AbstractDeclarator, Vec<StructDeclaration>) =
//                             self.parse(context)?;
//                         Ok((
//                             AbstractDeclarator::Pointer(Box::new(inner.0), true),
//                             inner.1,
//                         ))
//                     }
//                     _ => {
//                         self.expect(Token::KeywordRestrict)?;
//                         let inner: (AbstractDeclarator, Vec<StructDeclaration>) =
//                             self.parse(context)?;
//                         Ok((
//                             AbstractDeclarator::Pointer(Box::new(inner.0), false),
//                             inner.1,
//                         ))
//                     }
//                 }
//             }
//             _ => Ok(self.parse_direct(context)?),
//         }
//     }
// }
//
// trait ParseAbstractDeclarator {
//     fn parse_direct(
//         &mut self,
//         context: &mut ParseContext,
//     ) -> Result<(AbstractDeclarator, Vec<StructDeclaration>), Box<dyn Error>>;
//
//     fn parse_array(
//         &mut self,
//         a_declarator: AbstractDeclarator,
//         context: &mut ParseContext,
//     ) -> Result<(AbstractDeclarator, Vec<StructDeclaration>), Box<dyn Error>>;
// }
//
// impl ParseAbstractDeclarator for VecDeque<Token> {
//     fn parse_direct(
//         &mut self,
//         context: &mut ParseContext,
//     ) -> Result<(AbstractDeclarator, Vec<StructDeclaration>), Box<dyn Error>> {
//         match self.peek()? {
//             Token::OpenParen => {
//                 self.expect(Token::OpenParen)?;
//                 let (mut a, mut structs): (AbstractDeclarator, Vec<StructDeclaration>) =
//                     self.parse(context)?;
//                 self.expect(Token::CloseParen)?;
//                 while matches!(self.peek()?, Token::OpenSquareBracket) {
//                     let mut structs_from_array;
//                     (a, structs_from_array) = self.parse_array(a, context)?;
//                     structs.append(&mut structs_from_array);
//                 }
//                 Ok((a, structs))
//             }
//             Token::OpenSquareBracket => {
//                 let mut a = AbstractDeclarator::Base;
//                 let mut structs = Vec::new();
//                 while matches!(self.peek()?, Token::OpenSquareBracket) {
//                     let mut structs_from_array;
//                     (a, structs_from_array) = self.parse_array(a, context)?;
//                     structs.append(&mut structs_from_array);
//                 }
//                 Ok((a, structs))
//             }
//             _ => Ok((AbstractDeclarator::Base, Vec::new())),
//         }
//     }
//
//     fn parse_array(
//         &mut self,
//         mut a_declarator: AbstractDeclarator,
//         context: &mut ParseContext,
//     ) -> Result<(AbstractDeclarator, Vec<StructDeclaration>), Box<dyn Error>> {
//         self.expect(Token::OpenSquareBracket)?;
//
//         let e = self.parse(context)?;
//         a_declarator = AbstractDeclarator::Array(Box::new(a_declarator), e);
//
//         self.expect(Token::CloseSquareBracket)?;
//         Ok((a_declarator, Vec::new()))
//     }
// }
//
// impl AbstractDeclarator {
//     // NOTE: Okay so, turns out anonymous parameters in a function definition are full abstract
//     // declarators, so these 2 files will need to interact with each other a bunch depending on if
//     // a param is classes as anonymous or not. This will probably mean rewriting a lot of this
//     // function.
//     pub fn apply_to_type(
//         self,
//         base_type: Type,
//         context: &mut ParseContext,
//         // returns the type, name and list of parameter names associated with the type
//     ) -> Result<Type, Box<dyn Error>> {
//         match self {
//             AbstractDeclarator::Pointer(a, is_restricted) => {
//                 Ok(a.apply_to_type(Type::Pointer(Box::new(base_type), is_restricted), context)?)
//             }
//             AbstractDeclarator::Array(a, size_expression) => {
//                 let size = size_expression.fold_to_constant(context)?.value_unsigned();
//                 Ok(a.apply_to_type(Type::Array(Box::new(base_type), size), context)?)
//             }
//             AbstractDeclarator::Function(a, params) => {
//                 // struct_declarations.append(
//                 //     &mut params
//                 //         .iter()
//                 //         .filter_map(|(_, _, struct_declaration)| struct_declaration.clone())
//                 //         .collect(),
//                 // );
//                 let param_types: Vec<Type> = process_results(
//                     params.into_iter().map(|p_declarator| match p_declarator {
//                         ParamDeclarator::Plain(t, d) => d.apply_to_type(t),
//                         ParamDeclarator::Anonymous(a) =>
//                     }),
//                     |iter| iter.map(|o| (o.out_type).collect()),
//                 )?;
//
//                 let name = if let Declarator::Name(name) = *declarator {
//                     name
//                 } else {
//                     // discard param names, function pointers will never use them directly
//                     // because we know this isn't a function definition with a body.
//                     return declarator.apply_to_type(
//                         Type::Function(Box::new(base_type), param_types),
//                         struct_declarations,
//                         context,
//                     );
//                 };
//
//                 // Ok(DeclaratorApplicationOutput {
//                 //     out_type: Type::Function(Box::new(base_type), param_types),
//                 //     name,
//                 //     param_names: Some(param_names),
//                 //     struct_declarations,
//                 // })
//                 Ok(a.apply_to_type(
//                     Type::Function(Box::new(base_type), params),
//                     struct_declarations,
//                     context,
//                 )?)
//             }
//             AbstractDeclarator::Base => Ok(base_type),
//         }
//     }
// }
