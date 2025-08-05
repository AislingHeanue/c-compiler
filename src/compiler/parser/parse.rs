use itertools::{process_results, Itertools};

use super::{
    AbstractDeclarator, BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, Declarator,
    ExpressionNode, ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration,
    InitialiserNode, InitialiserWithoutType, Parse, ProgramNode, StatementNode, StorageClass, Type,
    UnaryOperatorNode, VariableDeclaration,
};
use crate::compiler::{lexer::Token, parser::ParseContext, types::Constant};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    mem::discriminant,
};

fn match_specifier(tokens: &VecDeque<Token>) -> Result<bool, Box<dyn Error>> {
    Ok(match_type(tokens)? || matches!(peek(tokens)?, Token::KeywordStatic | Token::KeywordExtern))
}

fn match_type(tokens: &VecDeque<Token>) -> Result<bool, Box<dyn Error>> {
    Ok(matches!(
        peek(tokens)?,
        Token::KeywordLong
            | Token::KeywordInt
            | Token::KeywordUnsigned
            | Token::KeywordSigned
            | Token::KeywordDouble
    ))
}

fn match_unary_operator(tokens: &VecDeque<Token>) -> Result<bool, Box<dyn Error>> {
    Ok(matches!(
        peek(tokens)?,
        Token::Hyphen | Token::Tilde | Token::Not | Token::Increment | Token::Decrement
    ))
}

fn match_suffix_operator(tokens: &VecDeque<Token>) -> Result<bool, Box<dyn Error>> {
    Ok(matches!(
        peek(tokens)?,
        Token::Increment | Token::Decrement | Token::OpenSquareBracket
    ))
}

fn match_constant(tokens: &VecDeque<Token>) -> Result<bool, Box<dyn Error>> {
    Ok(matches!(
        peek(tokens)?,
        Token::IntegerConstant(_)
            | Token::LongConstant(_)
            | Token::UnsignedIntegerConstant(_)
            | Token::UnsignedLongConstant(_)
            | Token::DoubleConstant(_)
    ))
}

fn match_assignment(tokens: &VecDeque<Token>) -> Result<bool, Box<dyn Error>> {
    Ok(matches!(
        peek(tokens)?,
        Token::Assignment
            | Token::AddAssign
            | Token::SubtractAssign
            | Token::MultiplyAssign
            | Token::DivideAssign
            | Token::ModAssign
            | Token::BitwiseAndAssign
            | Token::BitwiseXorAssign
            | Token::BitwiseOrAssign
            | Token::ShiftLeftAssign
            | Token::ShiftRightAssign
    ))
}

fn expect(tokens: &mut VecDeque<Token>, expected: Token) -> Result<(), Box<dyn Error>> {
    // println!("{:?}", expected);
    let token = read(tokens)?;
    if discriminant(&expected) != discriminant(&token) {
        return Err(format!(
            "Unexpected token, got {:?}, expecting {:?}",
            token, expected
        )
        .into());
    }
    Ok(())
}

fn read(tokens: &mut VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    // println!("popping {:?}", tokens.front());
    tokens
        .pop_front()
        .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())
}

fn peek(tokens: &VecDeque<Token>) -> Result<Token, Box<dyn Error>> {
    // println!("peeking {:?}", tokens.front());
    Ok(tokens
        .front()
        .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())?
        .clone())
}

fn new_identifier(
    name: String,
    is_file_scope: bool,
    context: &mut ParseContext,
    storage_class: Option<StorageClass>,
) -> Result<String, Box<dyn Error>> {
    let is_linked = is_file_scope || matches!(storage_class, Some(StorageClass::Extern));

    let new_name = if is_linked || context.do_not_validate {
        name.clone()
    } else {
        context.num_variables += 1;
        format!("{}.{}", name, context.num_variables)
    };

    if !context.do_not_validate {
        if let Some((_new_name, other_has_linkage)) = context.current_scope_identifiers.get(&name) {
            if !(*other_has_linkage && is_linked) {
                return Err(format!(
                    "Identifier named {} already exists in the current scope",
                    name
                )
                .into());
            }
        }
        // not entirely sure how this is compatible with the future type-checking step, but okay
        context
            .current_scope_identifiers
            .insert(name.clone(), (new_name.clone(), is_linked));
    }

    Ok(new_name)
}

fn enter_scope(context: &mut ParseContext) -> HashMap<String, (String, bool)> {
    // println!(
    //     "enter {:?} {:?}",
    //     context.outer_scope_identifiers, context.current_scope_identifiers
    // );
    let original_outer_scope_variables = context.outer_scope_identifiers.clone();
    context
        .outer_scope_identifiers
        .extend(context.current_scope_identifiers.clone());
    context.current_scope_identifiers = HashMap::new();

    original_outer_scope_variables
}

fn leave_scope(previous_scope: HashMap<String, (String, bool)>, context: &mut ParseContext) {
    context.current_scope_identifiers = context.outer_scope_identifiers.clone();
    context.outer_scope_identifiers = previous_scope;
    // println!(
    //     "leave {:?} {:?}",
    //     context.outer_scope_identifiers, context.current_scope_identifiers
    // );
}

impl Parse for ProgramNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let mut declarations: Vec<DeclarationNode> = Vec::new();
        while !tokens.is_empty() {
            declarations.push(DeclarationNode::parse(tokens, context)?)
        }

        Ok(ProgramNode { declarations })
    }
}

impl Parse for Type {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let mut out = Vec::new();
        while !tokens.is_empty() && match_type(tokens)? {
            out.push(read(tokens)?)
        }
        if out.is_empty() {
            return Err("No type tokens found".into());
        }
        let mut seen = Vec::new();
        for i in out.iter() {
            if seen.contains(i) {
                return Err("Repeated token in type definition".into());
            }
            seen.push(i.clone());
        }
        if out.contains(&Token::KeywordDouble) {
            if out.len() == 1 {
                return Ok(Type::Double);
            } else {
                return Err("Double cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordSigned) && out.contains(&Token::KeywordUnsigned) {
            return Err("Type specified as both signed and unsigned".into());
        }
        if out.contains(&Token::KeywordUnsigned) && out.contains(&Token::KeywordLong) {
            return Ok(Type::UnsignedLong);
        }
        if out.contains(&Token::KeywordUnsigned) {
            return Ok(Type::UnsignedInteger);
        }
        if out.contains(&Token::KeywordLong) {
            return Ok(Type::Long);
        }
        Ok(Type::Integer)
    }
}

impl Parse for Block {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        expect(tokens, Token::OpenBrace)?;

        // it's a new block it's a new scope
        // (and I'm feeling... good)
        let mut original_outer_scope_variables = None;
        // if this is a function body, this is not in fact a new scope, since the actual new scope
        // should also include the parameter list
        if !context.current_block_is_function_body {
            original_outer_scope_variables = Some(enter_scope(context));
        } else {
            context.current_scope_is_file = false;
        }

        context.current_block_is_function_body = false;

        let mut items: Vec<BlockItemNode> = Vec::new();
        while !matches!(peek(tokens)?, Token::CloseBrace) {
            items.push(BlockItemNode::parse(tokens, context)?)
        }

        if let Some(original) = original_outer_scope_variables {
            leave_scope(original, context);
        } else {
            context.current_scope_is_file = true;
        }

        expect(tokens, Token::CloseBrace)?;

        Ok(items)
    }
}

impl Parse for BlockItemNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        if tokens.is_empty() {
            return Err("Block item has no tokens".into());
        }

        if match_specifier(tokens)? {
            let declaration = DeclarationNode::parse(tokens, context)?;
            if let DeclarationNode::Function(ref f) = declaration {
                if f.body.is_some() && !context.do_not_validate {
                    return Err("Block-scope function declaration may not have a body".into());
                }
                if matches!(f.storage_class, Some(StorageClass::Static)) && !context.do_not_validate
                {
                    return Err("Block-scope function declaration may not be static".into());
                }
            }
            Ok(BlockItemNode::Declaration(declaration))
        } else {
            Ok(BlockItemNode::Statement(StatementNode::parse(
                tokens, context,
            )?))
        }
    }
}

impl Parse for ForInitialiserNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        if match_specifier(tokens)? {
            let block_item = BlockItemNode::parse(tokens, context)?;
            if let BlockItemNode::Declaration(DeclarationNode::Variable(v)) = block_item {
                if !context.do_not_validate && v.storage_class.is_some() {
                    return Err("For initialiser must not be declared as static or extern".into());
                }
                Ok(ForInitialiserNode::Declaration(v))
            } else if let BlockItemNode::Statement(StatementNode::Expression(expression)) =
                block_item
            {
                Ok(ForInitialiserNode::Expression(Some(expression)))
            } else {
                Err("Unexpected function declaration in the initialiser of a for loop".into())
            }
        } else {
            let expression =
                ForInitialiserNode::Expression(Option::<ExpressionNode>::parse(tokens, context)?);
            // pop the extra semi-colon off of tokens, since expressions themselves don't
            // expect semi-colons
            expect(tokens, Token::SemiColon)?;
            Ok(expression)
        }
    }
}

impl Parse for DeclarationNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let (base_type, storage_class) = <(Type, Option<StorageClass>)>::parse(tokens, context)?;

        let declarator = Declarator::parse(tokens, context)?;
        let declarator_output = declarator.apply_to_type(base_type)?;

        let out_type = declarator_output.out_type;
        let param_names = declarator_output.param_names.unwrap_or(Vec::new());

        match out_type {
            Type::Function(_, ref param_types) => {
                let name =
                    new_identifier(declarator_output.name, true, context, storage_class.clone())?;
                if param_names.len() != param_types.len() {
                    return Err("Mismatched length of parameter types and parameter names".into());
                }
                if !context.do_not_validate
                    && param_names.iter().unique().collect_vec().len() != param_names.len()
                {
                    return Err("Duplicate param name in function declaration".into());
                }

                if peek(tokens)? == Token::OpenBrace {
                    let original_outer_scope_variables = enter_scope(context);

                    // resolve param names into new identifiers tied to the current scope
                    let new_params_names = if !param_names.is_empty() {
                        let mut new_param_names = Vec::new();
                        for param_name in param_names {
                            new_param_names.push(new_identifier(param_name, false, context, None)?);
                        }
                        new_param_names
                    } else {
                        Vec::new()
                    };

                    // parse block in *not* a new scope
                    context.current_block_is_function_body = true;
                    let body = Block::parse(tokens, context)?;
                    leave_scope(original_outer_scope_variables, context);

                    Ok(DeclarationNode::Function(FunctionDeclaration {
                        function_type: out_type,
                        name,
                        params: new_params_names,
                        body: Some(body),
                        storage_class,
                    }))
                } else {
                    expect(tokens, Token::SemiColon)?;
                    Ok(DeclarationNode::Function(FunctionDeclaration {
                        function_type: out_type,
                        name,
                        params: param_names,
                        body: None,
                        storage_class,
                    }))
                }
            }
            _ => {
                let name = new_identifier(
                    declarator_output.name,
                    context.current_scope_is_file,
                    context,
                    storage_class.clone(),
                )?;
                // variable assignment
                match read(tokens)? {
                    Token::Assignment => {
                        // file scope variable = linkage
                        let initialiser = InitialiserNode::parse(tokens, context)?;
                        expect(tokens, Token::SemiColon)?;
                        Ok(DeclarationNode::Variable(VariableDeclaration {
                            variable_type: out_type,
                            name,
                            init: Some(initialiser),
                            storage_class,
                        }))
                    }
                    Token::SemiColon => Ok(DeclarationNode::Variable(VariableDeclaration {
                        variable_type: out_type,
                        name,
                        init: None,
                        storage_class,
                    })),
                    _ => Err("Invalid token in variable declaration".into()),
                }
            }
        }
    }
}

impl Parse for InitialiserNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let init = match peek(tokens)? {
            Token::OpenBrace => {
                expect(tokens, Token::OpenBrace)?;
                let mut initialisers = Vec::new();
                initialisers.push(InitialiserNode::parse(tokens, context)?);
                while matches!(peek(tokens)?, Token::Comma) {
                    expect(tokens, Token::Comma)?;
                    if matches!(peek(tokens)?, Token::CloseParen) {
                        //baited, no new init here
                        break;
                    }
                    initialisers.push(InitialiserNode::parse(tokens, context)?);
                }
                expect(tokens, Token::CloseBrace)?;
                InitialiserWithoutType::Compound(initialisers)
            }
            _ => InitialiserWithoutType::Single(ExpressionNode::parse(tokens, context)?),
        };
        Ok(init.into())
    }
}

impl From<InitialiserWithoutType> for InitialiserNode {
    fn from(val: InitialiserWithoutType) -> Self {
        InitialiserNode(val, None)
    }
}

impl Parse for Declarator {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::Star => {
                expect(tokens, Token::Star)?;
                Ok(Declarator::Pointer(Box::new(Declarator::parse(
                    tokens, context,
                )?)))
            }
            _ => {
                let mut simple_declarator = Declarator::parse_simple_declarator(tokens, context)?;
                match peek(tokens)? {
                    // function declaration type!
                    Token::OpenParen => {
                        let param_list = Declarator::parse_param_list(tokens, context)?;
                        Ok(Declarator::Function(
                            Box::new(simple_declarator),
                            param_list,
                        ))
                    }
                    Token::OpenSquareBracket => {
                        while matches!(peek(tokens)?, Token::OpenSquareBracket) {
                            simple_declarator =
                                Declarator::parse_array(tokens, simple_declarator, context)?;
                        }
                        Ok(simple_declarator)
                    }
                    _ => Ok(simple_declarator),
                }
            }
        }
    }
}

#[derive(Debug)]
struct DeclaratorApplicationOutput {
    name: String,
    out_type: Type,
    param_names: Option<Vec<String>>,
}

impl Declarator {
    fn parse_simple_declarator(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        match read(tokens)? {
            Token::Identifier(name) => {
                // do not process the scope of 'name' here, because it complicates parameters
                Ok(Declarator::Name(name))
            }
            Token::OpenParen => {
                let declarator = Declarator::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Ok(declarator)
            }
            _ => Err("Invalid declarator".into()),
        }
    }

    fn parse_param_list(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Vec<(Type, Declarator)>, Box<dyn Error>> {
        expect(tokens, Token::OpenParen)?;
        let mut param_list = Vec::new();

        if peek(tokens)? == Token::KeywordVoid {
            expect(tokens, Token::KeywordVoid)?;
            expect(tokens, Token::CloseParen)?;
            return Ok(param_list);
        }

        param_list.push(Declarator::parse_param(tokens, context)?);
        while peek(tokens)? != Token::CloseParen {
            expect(tokens, Token::Comma)?;
            param_list.push(Declarator::parse_param(tokens, context)?);
        }
        expect(tokens, Token::CloseParen)?;
        Ok(param_list)
    }

    fn parse_param(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<(Type, Declarator), Box<dyn Error>> {
        let out_type = Type::parse(tokens, context)?;
        // function params never have static or extern storage
        let declarator = Declarator::parse(tokens, context)?;
        Ok((out_type, declarator))
    }

    fn parse_array(
        tokens: &mut VecDeque<Token>,
        mut declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        expect(tokens, Token::OpenSquareBracket)?;
        let c = Constant::parse(tokens, context)?;
        let maybe_i: Option<i32> = match c {
            Constant::Integer(i) => Some(i),
            Constant::Long(i) => i.try_into().ok(),
            Constant::UnsignedInteger(i) => i.try_into().ok(),
            Constant::UnsignedLong(i) => i.try_into().ok(),
            _ => None,
        };
        if let Some(i) = maybe_i {
            if i < 1 {
                return Err("Array dimension must be at least 1".into());
            }
            declarator = Declarator::Array(Box::new(declarator), i);
        } else {
            return Err("Constant value in array type must be an integer".into());
        }
        expect(tokens, Token::CloseSquareBracket)?;
        Ok(declarator)
    }

    fn apply_to_type(
        self,
        base_type: Type,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<DeclaratorApplicationOutput, Box<dyn Error>> {
        match self {
            Declarator::Name(name) => Ok(DeclaratorApplicationOutput {
                out_type: base_type,
                name,
                param_names: None,
            }),
            Declarator::Pointer(declarator) => {
                // discard param names, function pointers aren't real
                declarator.apply_to_type(Type::Pointer(Box::new(base_type)))
            }
            Declarator::Function(declarator, params) => {
                let name = if let Declarator::Name(name) = *declarator {
                    name
                } else {
                    return Err("Cannot apply additional declarators to a function type (function pointers aren't real)".into());
                };
                let (param_types, param_names): (Vec<Type>, Vec<String>) = process_results(
                    params
                        .into_iter()
                        .map(|(t, declarator)| declarator.apply_to_type(t)),
                    |iter| iter.map(|o| (o.out_type, o.name)).unzip(),
                )?;

                Ok(DeclaratorApplicationOutput {
                    out_type: Type::Function(Box::new(base_type), param_types),
                    name,
                    param_names: Some(param_names),
                })
            }
            Declarator::Array(declarator, size) => {
                declarator.apply_to_type(Type::Array(Box::new(base_type), size))
            }
        }
    }
}

impl Parse for AbstractDeclarator {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::Star => {
                expect(tokens, Token::Star)?;
                Ok(AbstractDeclarator::Pointer(Box::new(
                    AbstractDeclarator::parse(tokens, _context)?,
                )))
            }
            _ => Ok(AbstractDeclarator::parse_direct(tokens, _context)?),
        }
    }
}

impl AbstractDeclarator {
    fn parse_direct(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::OpenParen => {
                expect(tokens, Token::OpenParen)?;
                let mut a = AbstractDeclarator::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                while matches!(peek(tokens)?, Token::OpenSquareBracket) {
                    a = AbstractDeclarator::parse_array(tokens, a, context)?;
                }
                Ok(a)
            }
            _ => Ok(AbstractDeclarator::Base),
        }
    }

    fn parse_array(
        tokens: &mut VecDeque<Token>,
        mut a_declarator: AbstractDeclarator,
        context: &mut ParseContext,
    ) -> Result<AbstractDeclarator, Box<dyn Error>> {
        expect(tokens, Token::OpenSquareBracket)?;
        let c = Constant::parse(tokens, context)?;
        let maybe_i: Option<i32> = match c {
            Constant::Integer(i) => Some(i),
            Constant::Long(i) => i.try_into().ok(),
            Constant::UnsignedInteger(i) => i.try_into().ok(),
            Constant::UnsignedLong(i) => i.try_into().ok(),
            _ => None,
        };
        if let Some(i) = maybe_i {
            if i < 1 {
                return Err("Array dimension must be at least 1".into());
            }
            a_declarator = AbstractDeclarator::Array(Box::new(a_declarator), i);
        } else {
            return Err("Constant value in array type must be an integer".into());
        }
        expect(tokens, Token::CloseSquareBracket)?;
        Ok(a_declarator)
    }

    fn apply_to_type(
        self,
        base_type: Type,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<Type, Box<dyn Error>> {
        match self {
            AbstractDeclarator::Pointer(a) => Ok(Type::Pointer(Box::new(
                AbstractDeclarator::apply_to_type(*a, base_type)?,
            ))),
            AbstractDeclarator::Array(a, size) => Ok(Type::Array(
                Box::new(AbstractDeclarator::apply_to_type(*a, base_type)?),
                size,
            )),
            AbstractDeclarator::Base => Ok(base_type),
        }
    }
}

impl Parse for StatementNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::KeywordReturn => {
                expect(tokens, Token::KeywordReturn)?;
                let expression = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Return(expression))
            }
            Token::KeywordIf => {
                expect(tokens, Token::KeywordIf)?;
                expect(tokens, Token::OpenParen)?;
                let condition = ExpressionNode::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let then = StatementNode::parse(tokens, context)?;
                let otherwise = match peek(tokens)? {
                    Token::KeywordElse => {
                        expect(tokens, Token::KeywordElse)?;
                        Some(StatementNode::parse(tokens, context)?)
                    }
                    _ => None,
                };
                Ok(StatementNode::If(
                    condition,
                    Box::new(then),
                    Box::new(otherwise),
                ))
            }
            Token::SemiColon => {
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Pass)
            }
            Token::KeywordGoto => {
                expect(tokens, Token::KeywordGoto)?;
                let s = match read(tokens)? {
                    Token::Identifier(s) => Ok::<String, Box<dyn Error>>(s),
                    t => Err(format!("unexpected token in goto: {:?}", t).into()),
                }?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Goto(s))
            }
            Token::OpenBrace => {
                let block = Block::parse(tokens, context)?;
                Ok(StatementNode::Compound(block))
            }
            Token::KeywordFor => {
                expect(tokens, Token::KeywordFor)?;
                expect(tokens, Token::OpenParen)?;

                // create a new scope just for the first line of the 'for' declaration
                let outer_scope = enter_scope(context);

                let init = ForInitialiserNode::parse(tokens, context)?;
                let cond = Option::<ExpressionNode>::parse(tokens, context)?;
                expect(tokens, Token::SemiColon)?;
                let post = Option::<ExpressionNode>::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let body = StatementNode::parse(tokens, context)?;

                leave_scope(outer_scope, context);
                Ok(StatementNode::For(init, cond, post, Box::new(body), None))
            }
            Token::KeywordDo => {
                expect(tokens, Token::KeywordDo)?;
                let body = StatementNode::parse(tokens, context)?;
                expect(tokens, Token::KeywordWhile)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::DoWhile(
                    Box::new(body),
                    expression.into(),
                    None,
                ))
            }
            Token::KeywordWhile => {
                expect(tokens, Token::KeywordWhile)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                let body = StatementNode::parse(tokens, context)?;
                Ok(StatementNode::While(
                    expression.into(),
                    Box::new(body),
                    None,
                ))
            }
            Token::KeywordBreak => {
                expect(tokens, Token::KeywordBreak)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Break(None))
            }
            Token::KeywordContinue => {
                expect(tokens, Token::KeywordContinue)?;
                expect(tokens, Token::SemiColon)?;
                Ok(StatementNode::Continue(None))
            }
            Token::KeywordSwitch => {
                expect(tokens, Token::KeywordSwitch)?;
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Ok(StatementNode::Switch(
                    expression.into(),
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                    None,
                ))
            }
            Token::KeywordCase => {
                expect(tokens, Token::KeywordCase)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::Colon)?;
                Ok(StatementNode::Case(
                    expression.into(),
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                ))
            }
            Token::KeywordDefault => {
                expect(tokens, Token::KeywordDefault)?;
                expect(tokens, Token::Colon)?;
                Ok(StatementNode::Default(
                    Box::new(StatementNode::parse(tokens, context)?),
                    None,
                ))
            }
            _ => match (
                peek(tokens)?,
                tokens
                    .get(1)
                    .ok_or::<Box<dyn Error>>("Statement node only has one token".into())?,
            ) {
                (Token::Identifier(s), Token::Colon) => {
                    expect(tokens, Token::Identifier("".to_string()))?;
                    expect(tokens, Token::Colon)?;
                    Ok(StatementNode::Label(
                        s.to_string(),
                        Box::new(StatementNode::parse(tokens, context)?),
                    ))
                }
                _ => {
                    let expression = ExpressionNode::parse(tokens, context)?;
                    expect(tokens, Token::SemiColon)?;
                    Ok(StatementNode::Expression(expression))
                }
            },
        }
    }
}

impl Parse for Option<ExpressionNode> {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(
            ExpressionWithoutType::parse_with_level(tokens, context, 0, true)?
                .map(|res| res.into()),
        )
    }
}

impl Parse for ExpressionNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(ExpressionWithoutType::parse(tokens, context)?.into())
    }
}

impl Parse for ExpressionWithoutType {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(ExpressionWithoutType::parse_with_level(tokens, context, 0, false)?.unwrap())
    }
}

impl From<ExpressionWithoutType> for ExpressionNode {
    fn from(val: ExpressionWithoutType) -> Self {
        ExpressionNode(val, None)
    }
}

impl ExpressionWithoutType {
    fn parse_with_level(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        level: usize,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let maybe_left = ExpressionWithoutType::parse_unary(tokens, context, allow_empty)?;
        if maybe_left.is_none() {
            // if allow_empty is false, parse_unary will throw the relevant error for here
            return Ok(None);
        }
        let mut left = maybe_left.unwrap();
        while let Some(precedence) = BinaryOperatorNode::precedence(tokens)? {
            if precedence < level {
                break;
            }
            if match_assignment(tokens)? {
                let operator_token = read(tokens)?;
                let right =
                    ExpressionWithoutType::parse_with_level(tokens, context, precedence, false)?;
                left = if let Token::Assignment = operator_token {
                    ExpressionWithoutType::Assignment(
                        Box::new(left.into()),
                        Box::new(right.unwrap().into()),
                    )
                } else {
                    let operator = match operator_token {
                        Token::AddAssign => BinaryOperatorNode::Add,
                        Token::SubtractAssign => BinaryOperatorNode::Subtract,
                        Token::MultiplyAssign => BinaryOperatorNode::Multiply,
                        Token::DivideAssign => BinaryOperatorNode::Divide,
                        Token::ModAssign => BinaryOperatorNode::Mod,
                        Token::BitwiseAndAssign => BinaryOperatorNode::BitwiseAnd,
                        Token::BitwiseXorAssign => BinaryOperatorNode::BitwiseXor,
                        Token::BitwiseOrAssign => BinaryOperatorNode::BitwiseOr,
                        Token::ShiftLeftAssign => BinaryOperatorNode::ShiftLeft,
                        Token::ShiftRightAssign => BinaryOperatorNode::ShiftRight,
                        _ => {
                            unreachable!("Can't use {:?} as an assignment operator", operator_token)
                        }
                    };
                    ExpressionWithoutType::Compound(
                        operator,
                        Box::new(left.into()),
                        Box::new(right.unwrap().into()),
                    )
                };
            } else {
                match peek(tokens)? {
                    Token::Question => {
                        expect(tokens, Token::Question)?;
                        // parse this expression with precedence level reset
                        let middle = ExpressionWithoutType::parse(tokens, context)?;
                        expect(tokens, Token::Colon)?;
                        let end = ExpressionWithoutType::parse_with_level(
                            tokens, context, precedence, false,
                        )?;
                        left = ExpressionWithoutType::Ternary(
                            Box::new(left.into()),
                            Box::new(middle.into()),
                            Box::new(end.unwrap().into()),
                        )
                    }
                    _ => {
                        left = ExpressionWithoutType::Binary(
                            BinaryOperatorNode::parse(tokens, context)?,
                            Box::new(left.into()),
                            Box::new(
                                ExpressionWithoutType::parse_with_level(
                                    tokens,
                                    context,
                                    precedence + 1,
                                    false,
                                )?
                                .unwrap()
                                .into(),
                            ),
                        );
                    }
                }
            }
        }

        Ok(Some(left))
    }

    fn parse_unary(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let expression = {
            match peek(tokens)? {
                Token::OpenParen => {
                    // casting
                    expect(tokens, Token::OpenParen)?;
                    if match_type(tokens)? {
                        let cast_type = Type::parse(tokens, context)?;
                        let abstract_declarator = AbstractDeclarator::parse(tokens, context)?;
                        let real_cast_type = abstract_declarator.apply_to_type(cast_type)?;
                        expect(tokens, Token::CloseParen)?;
                        let factor =
                            ExpressionWithoutType::parse_unary(tokens, context, false)?.unwrap();
                        Some(ExpressionWithoutType::Cast(
                            real_cast_type,
                            Box::new(factor.into()),
                        ))
                    } else {
                        // baited, not actually a cast, go further down the chain...
                        tokens.push_front(Token::OpenParen);
                        let expression =
                            ExpressionWithoutType::parse_postfix(tokens, context, allow_empty)?
                                .unwrap();
                        Some(expression)
                    }
                }
                // address-of
                Token::BitwiseAnd => {
                    expect(tokens, Token::BitwiseAnd)?;
                    let precedence = UnaryOperatorNode::precedence();
                    let inner_expression = ExpressionWithoutType::parse_with_level(
                        tokens, context, precedence, false,
                    )?;
                    Some(ExpressionWithoutType::AddressOf(Box::new(
                        inner_expression.unwrap().into(),
                    )))
                }
                // deref
                Token::Star => {
                    expect(tokens, Token::Star)?;
                    let precedence = UnaryOperatorNode::precedence();
                    let inner_expression = ExpressionWithoutType::parse_with_level(
                        tokens, context, precedence, false,
                    )?;
                    Some(ExpressionWithoutType::Dereference(Box::new(
                        inner_expression.unwrap().into(),
                    )))
                }
                _ if match_unary_operator(tokens)? => {
                    let operator = UnaryOperatorNode::parse(tokens, context)?;
                    let precedence = UnaryOperatorNode::precedence(); // all unary operators have the
                                                                      // same precedence
                    let expression = ExpressionWithoutType::parse_with_level(
                        tokens, context, precedence, false,
                    )?;
                    Some(ExpressionWithoutType::Unary(
                        operator,
                        Box::new(expression.unwrap().into()),
                    ))
                }
                _ => ExpressionWithoutType::parse_postfix(tokens, context, allow_empty)?,
            }
        };
        Ok(expression)
    }

    fn parse_postfix(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let mut left = ExpressionWithoutType::parse_primary(tokens, context, allow_empty)?;
        if left.is_none() {
            return Ok(left);
        }
        while match_suffix_operator(tokens)? {
            match peek(tokens)? {
                Token::Increment | Token::Decrement => {
                    left = Some(ExpressionWithoutType::Unary(
                        UnaryOperatorNode::parse_as_suffix(tokens, context)?,
                        Box::new(left.unwrap().into()),
                    ));
                }
                Token::OpenSquareBracket => {
                    expect(tokens, Token::OpenSquareBracket)?;
                    let inner = ExpressionWithoutType::parse(tokens, context)?;
                    expect(tokens, Token::CloseSquareBracket)?;
                    left = Some(ExpressionWithoutType::Subscript(
                        Box::new(left.unwrap().into()),
                        Box::new(inner.into()),
                    ))
                }
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_primary(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let expression = match peek(tokens)? {
            Token::Identifier(name) => {
                expect(tokens, Token::Identifier("".to_string()))?;
                match peek(tokens)? {
                    // this is a function call !!
                    Token::OpenParen => {
                        let (new_name, _external_link) =
                            ExpressionWithoutType::resolve_identifier(&name, context)?;

                        expect(tokens, Token::OpenParen)?;
                        let mut arguments: Vec<ExpressionWithoutType> = Vec::new();
                        if !matches!(peek(tokens)?, Token::CloseParen) {
                            arguments.push(ExpressionWithoutType::parse(tokens, context)?);
                        }
                        while matches!(peek(tokens)?, Token::Comma) {
                            expect(tokens, Token::Comma)?;
                            arguments.push(ExpressionWithoutType::parse(tokens, context)?);
                        }
                        expect(tokens, Token::CloseParen)?;

                        Some(ExpressionWithoutType::FunctionCall(
                            new_name,
                            arguments.into_iter().map(|a| a.into()).collect(),
                        ))
                    }
                    _ => {
                        // VALIDATION STEP: Check the variable has been declared
                        let (new_name, _external_link) =
                            ExpressionWithoutType::resolve_identifier(&name, context)?;

                        Some(ExpressionWithoutType::Var(new_name))
                    }
                }
            }
            Token::OpenParen => {
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Some(expression)
            }
            _ if match_constant(tokens)? => Some(ExpressionWithoutType::Constant(Constant::parse(
                tokens, context,
            )?)),
            t => {
                if allow_empty {
                    None
                } else {
                    return Err(format!("Invalid token at start of expression: {:?}", t).into());
                }
            }
        };

        Ok(expression)
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            ExpressionWithoutType::Var(_) | ExpressionWithoutType::Dereference(_)
        )
    }

    fn resolve_identifier(
        name: &str,
        context: &mut ParseContext,
    ) -> Result<(String, bool), Box<dyn Error>> {
        // println!(
        //     "resolve {:?} {:?}",
        //     context.outer_scope_identifiers, context.current_scope_identifiers
        // );
        if context.do_not_validate {
            Ok((name.to_string(), false))
        } else if let Some(new_name) = context.current_scope_identifiers.get(name) {
            Ok(new_name.clone())
        } else if let Some(new_name) = context.outer_scope_identifiers.get(name) {
            Ok(new_name.clone())
        } else {
            Err(format!("Identifier used before declaration: {}", name).into())
        }
    }
}

impl Parse for Constant {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match read(tokens)? {
            Token::IntegerConstant(v) => Ok(if v <= i32::MAX.into() {
                Constant::Integer(v.try_into().unwrap())
            } else {
                Constant::Long(v)
            }),
            Token::UnsignedIntegerConstant(v) => Ok(if v <= u32::MAX.into() {
                Constant::UnsignedInteger(v.try_into().unwrap())
            } else {
                Constant::UnsignedLong(v)
            }),
            Token::LongConstant(v) => Ok(Constant::Long(v)),
            Token::UnsignedLongConstant(v) => Ok(Constant::UnsignedLong(v)),
            Token::DoubleConstant(v) => Ok(Constant::Double(v)),
            _ => unreachable!(),
        }
    }
}

impl Parse for UnaryOperatorNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match read(tokens)? {
            Token::Hyphen => Ok(UnaryOperatorNode::Negate),
            Token::Tilde => Ok(UnaryOperatorNode::Complement),
            Token::Not => Ok(UnaryOperatorNode::Not),
            Token::Increment => Ok(UnaryOperatorNode::PrefixIncrement),
            Token::Decrement => Ok(UnaryOperatorNode::PrefixDecrement),
            t => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                t,
            )
            .into()),
        }
    }
}

impl UnaryOperatorNode {
    fn parse_as_suffix(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match read(tokens)? {
            Token::Increment => Ok(UnaryOperatorNode::SuffixIncrement),
            Token::Decrement => Ok(UnaryOperatorNode::SuffixDecrement),
            t => Err(format!(
                "Unexpected token, got: {:?}, expecting identifier or operator token",
                t,
            )
            .into()),
        }
    }

    fn precedence() -> usize {
        55
    }
}

impl Parse for BinaryOperatorNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(match read(tokens)? {
            Token::Star => BinaryOperatorNode::Multiply,
            Token::Slash => BinaryOperatorNode::Divide,
            Token::Percent => BinaryOperatorNode::Mod,

            Token::Plus => BinaryOperatorNode::Add,
            Token::Hyphen => BinaryOperatorNode::Subtract,

            Token::ShiftLeft => BinaryOperatorNode::ShiftLeft,
            Token::ShiftRight => BinaryOperatorNode::ShiftRight,

            Token::Less => BinaryOperatorNode::Less,
            Token::LessEqual => BinaryOperatorNode::LessEqual,
            Token::Greater => BinaryOperatorNode::Greater,
            Token::GreaterEqual => BinaryOperatorNode::GreaterEqual,

            Token::Equal => BinaryOperatorNode::Equal,
            Token::NotEqual => BinaryOperatorNode::NotEqual,

            Token::BitwiseAnd => BinaryOperatorNode::BitwiseAnd,
            Token::BitwiseXor => BinaryOperatorNode::BitwiseXor,
            Token::BitwiseOr => BinaryOperatorNode::BitwiseOr,

            Token::And => BinaryOperatorNode::And,

            Token::Or => BinaryOperatorNode::Or,

            a => return Err(format!("token is not a binary operation: {:?}", a).into()),
        })
    }
}
impl BinaryOperatorNode {
    fn precedence(tokens: &mut VecDeque<Token>) -> Result<Option<usize>, Box<dyn Error>> {
        Ok(Some(match peek(tokens)? {
            Token::Increment => 60,
            Token::Decrement => 60,

            Token::Star => 50,
            Token::Slash => 50,
            Token::Percent => 50,

            Token::Plus => 45,
            Token::Hyphen => 45,

            Token::ShiftLeft => 40,
            Token::ShiftRight => 40,

            Token::Less => 35,
            Token::LessEqual => 35,
            Token::Greater => 35,
            Token::GreaterEqual => 35,

            Token::Equal => 30,
            Token::NotEqual => 30,

            Token::BitwiseAnd => 25,
            Token::BitwiseXor => 20,
            Token::BitwiseOr => 15,

            Token::And => 10,

            Token::Or => 5,

            Token::Question => 3,

            Token::Assignment => 1,
            Token::AddAssign => 1,
            Token::SubtractAssign => 1,
            Token::MultiplyAssign => 1,
            Token::DivideAssign => 1,
            Token::ModAssign => 1,
            Token::BitwiseAndAssign => 1,
            Token::BitwiseXorAssign => 1,
            Token::BitwiseOrAssign => 1,
            Token::ShiftLeftAssign => 1,
            Token::ShiftRightAssign => 1,

            _ => return Ok(None),
        }))
    }
}

impl Parse for (Type, Option<StorageClass>) {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        // Specifiers may be one of "static", "int" or "extern"
        // ... and there may be any non-zero amount of them
        let mut storage_classes = Vec::new();
        let mut types = Vec::new();
        while match_specifier(tokens)? {
            if match_type(tokens)? {
                types.push(read(tokens)?);
            } else {
                storage_classes.push(read(tokens)?);
            }
        }

        if storage_classes.len() > 1 {
            return Err(format!(
                "Got multiple storage classes in declaration: {:?}",
                storage_classes
            )
            .into());
        }

        let storage = storage_classes.first().map(|t| match t {
            Token::KeywordStatic => StorageClass::Static,
            Token::KeywordExtern => StorageClass::Extern,
            _ => unreachable!(),
        });

        let this_type = Type::parse(&mut types.into(), context)?;
        Ok((this_type, storage))
    }
}
