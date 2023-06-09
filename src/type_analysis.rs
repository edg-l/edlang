use std::collections::{HashMap, HashSet};

use tracing::{info, warn};

use crate::ast::{self, Expression, Function, Statement, TypeExp};

#[derive(Debug, Clone, Default)]
struct Storage {
    structs: HashMap<String, HashMap<String, TypeExp>>,
    functions: HashMap<String, Function>,
}

// this works, but need to find a way to store the found info + handle literal integer types (or not?)
pub fn type_inference2(ast: &mut ast::Program) {
    let mut storage = Storage::default();

    // gather global constructs first
    for statement in ast.statements.iter_mut() {
        match statement {
            Statement::Struct(st) => {
                let fields = st
                    .fields
                    .iter()
                    .map(|x| (x.ident.clone(), x.type_exp.clone()))
                    .collect();
                storage.structs.insert(st.name.clone(), fields);
            }
            Statement::Function(function) => {
                storage
                    .functions
                    .insert(function.name.clone(), function.clone());
            }
            // todo: find globals here too
            _ => {}
        }
    }

    dbg!(&storage);

    for function in storage.functions.values() {
        let mut scope_vars: HashMap<String, Option<TypeExp>> = HashMap::new();

        for arg in &function.params {
            scope_vars.insert(arg.ident.clone(), Some(arg.type_exp.clone()));
        }

        let (new_scope_vars, _) =
            type_inference_scope(&function.body, &scope_vars, function, &storage);
        dbg!(new_scope_vars);
    }
}

/// Finds variable types in the scope, returns newly created variables to handle shadowing
fn type_inference_scope(
    statements: &[ast::Statement],
    scope_vars: &HashMap<String, Option<TypeExp>>,
    func: &Function,
    storage: &Storage,
) -> (HashMap<String, Option<TypeExp>>, HashSet<String>) {
    let mut scope_vars = scope_vars.clone();
    let mut new_vars: HashSet<String> = HashSet::new();

    for statement in statements {
        match statement {
            Statement::Let {
                name,
                value,
                value_type,
                span: _,
            } => {
                new_vars.insert(name.clone());

                let exp_type = type_inference_expression(value, &mut scope_vars, storage, None);

                if value_type.is_none() {
                    scope_vars.insert(name.clone(), exp_type);
                } else {
                    if exp_type.is_some() && &exp_type != value_type {
                        panic!("let type mismatch: {:?} != {:?}", value_type, exp_type);
                    }
                    scope_vars.insert(name.clone(), value_type.clone());
                }
            }
            Statement::Mutate {
                name,
                value,
                value_type: _,
                span: _,
            } => {
                if !scope_vars.contains_key(name) {
                    panic!("undeclared variable");
                }

                let exp_type = type_inference_expression(value, &mut scope_vars, storage, None);
                let var = scope_vars.get_mut(name).unwrap();

                if var.is_none() {
                    *var = exp_type;
                } else if exp_type.is_some() && &exp_type != var {
                    panic!("mutate type mismatch: {:?} != {:?}", var, exp_type);
                }
            }
            Statement::If {
                condition,
                body,
                else_body,
            } => {
                type_inference_expression(
                    condition,
                    &mut scope_vars,
                    storage,
                    Some(TypeExp::Boolean),
                );

                let (new_scope_vars, new_vars) =
                    type_inference_scope(body, &scope_vars, func, storage);

                for (k, v) in new_scope_vars.into_iter() {
                    // not a new var within the scope (shadowing), so type info is valid
                    if scope_vars.contains_key(&k) && !new_vars.contains(&k) {
                        scope_vars.insert(k, v);
                    }
                }

                if let Some(body) = else_body {
                    let (new_scope_vars, new_vars) =
                        type_inference_scope(body, &scope_vars, func, storage);

                    for (k, v) in new_scope_vars.into_iter() {
                        // not a new var within the scope (shadowing), so type info is valid
                        if scope_vars.contains_key(&k) && !new_vars.contains(&k) {
                            scope_vars.insert(k, v);
                        }
                    }
                }
            }
            Statement::Return(exp) => {
                if let Some(exp) = exp {
                    type_inference_expression(
                        exp,
                        &mut scope_vars,
                        storage,
                        func.return_type.clone(),
                    );
                }
            }
            Statement::Function(_) => unreachable!(),
            Statement::Struct(_) => unreachable!(),
        }
    }

    (scope_vars, new_vars)
}

fn type_inference_expression(
    exp: &Expression,
    scope_vars: &mut HashMap<String, Option<TypeExp>>,
    storage: &Storage,
    expected_type: Option<TypeExp>,
) -> Option<TypeExp> {
    match exp {
        Expression::Literal(lit) => {
            match lit {
                ast::LiteralValue::String(_) => None, // todo
                ast::LiteralValue::Integer {
                    value: _,
                    bits,
                    signed,
                } => {
                    if bits.is_some() && signed.is_some() {
                        Some(TypeExp::Integer {
                            bits: bits.unwrap(),
                            signed: signed.unwrap(),
                        })
                    } else {
                        None
                    }
                }
                ast::LiteralValue::Boolean(_) => Some(TypeExp::Boolean),
            }
        }
        Expression::Variable {
            name,
            value_type: _,
        } => {
            let var = scope_vars.get(&name.value).cloned().flatten();

            if expected_type.is_some() {
                if var.is_none() {
                    scope_vars.insert(name.value.clone(), expected_type.clone());
                    expected_type
                } else if expected_type.is_some() {
                    assert_eq!(var, expected_type, "type mismatch with variables");
                    expected_type
                } else {
                    var
                }
            } else {
                var
            }
        }
        Expression::Call {
            function,
            args,
            value_type: _,
        } => {
            let func = storage.functions.get(function).cloned().unwrap();

            for (i, arg) in args.iter().enumerate() {
                let arg_type = func.params[i].type_exp.clone();
                // result is ignored, but need these to infer call arg types
                type_inference_expression(arg, scope_vars, storage, Some(arg_type));
            }

            func.return_type
        }
        Expression::BinaryOp(lhs, op, rhs) => match op {
            ast::OpCode::Eq | ast::OpCode::Ne => Some(TypeExp::Boolean),
            _ => {
                let lhs_type =
                    type_inference_expression(lhs, scope_vars, storage, expected_type.clone());
                let rhs_type = type_inference_expression(rhs, scope_vars, storage, expected_type);

                if lhs_type.is_some() && rhs_type.is_some() {
                    assert_eq!(lhs_type, rhs_type, "types should match");
                }

                lhs_type.or(rhs_type)
            }
        },
    }
}

pub fn type_inference(ast: &mut ast::Program) {
    let mut struct_cache: HashMap<String, HashMap<String, TypeExp>> = HashMap::new();
    for statement in ast.statements.iter_mut() {
        if let Statement::Struct(st) = statement {
            let fields = st
                .fields
                .iter()
                .map(|x| (x.ident.clone(), x.type_exp.clone()))
                .collect();
            struct_cache.insert(st.name.clone(), fields);
        }
    }

    let mut fn_cache: HashMap<String, Function> = HashMap::new();
    for statement in ast.statements.iter_mut() {
        if let Statement::Function(function) = statement {
            fn_cache.insert(function.name.clone(), function.clone());
        }
    }

    for statement in ast.statements.iter_mut() {
        if let Statement::Function(function) = statement {
            let ret_type = function.return_type.clone();
            let mut var_cache: HashMap<String, TypeExp> = HashMap::new();

            for arg in &function.params {
                var_cache.insert(arg.ident.clone(), arg.type_exp.clone());
            }

            if let Some(ret_type) = &ret_type {
                let ret_type_exp = fn_return_type(function);

                if let Some(exp) = ret_type_exp {
                    set_expression_type(exp, ret_type, &mut var_cache);
                }
            }

            update_statements(&mut function.body, &mut var_cache, &fn_cache);
        }
    }
}

fn update_statements(
    statements: &mut [Statement],
    var_cache: &mut HashMap<String, TypeExp>,
    fn_cache: &HashMap<String, Function>,
) {
    let mut var_cache = var_cache.clone();

    {
        let mut let_or_mut: Vec<&mut Statement> = statements
            .iter_mut()
            .filter(|x| matches!(x, Statement::Let { .. } | Statement::Mutate { .. }))
            .collect();

        // process mutate first
        for st in let_or_mut.iter_mut() {
            if let Statement::Mutate {
                name,
                value,
                value_type,
                ..
            } = st
            {
                if let Some(value_type) = value_type {
                    // todo: check types matches?
                    var_cache.insert(name.clone(), value_type.clone());
                    set_expression_type(value, value_type, &mut var_cache);
                } else {
                    // evalue the value expr first to find a possible type.
                    if var_cache.contains_key(name) {
                        *value_type = var_cache.get(name).cloned();
                        let mut env = Some(value_type.clone().unwrap());
                        set_exp_types_from_cache(value, &mut var_cache, &mut env, fn_cache);
                    } else {
                        // no type info?
                    }
                }
            }
        }

        // we need to process lets with a specified type first.
        for st in let_or_mut.iter_mut() {
            if let Statement::Let {
                name,
                value,
                value_type,
                ..
            } = st
            {
                if let Some(value_type) = value_type {
                    // todo: check types matches?
                    var_cache.insert(name.clone(), value_type.clone());
                    set_expression_type(value, value_type, &mut var_cache);
                } else {
                    // evalue the value expr first to find a possible type.
                    if var_cache.contains_key(name) {
                        *value_type = var_cache.get(name).cloned();
                        let mut env = Some(value_type.clone().unwrap());
                        set_exp_types_from_cache(value, &mut var_cache, &mut env, fn_cache);
                    } else {
                        // no type info?
                    }
                }
            }
        }
    }

    for st in statements.iter_mut() {
        match st {
            Statement::Let {
                name,
                value_type,
                value,
                ..
            } => {
                // infer type if let has no type
                if value_type.is_none() {
                    // evalue the value expr first to find a possible type.
                    let mut env = None;
                    set_exp_types_from_cache(value, &mut var_cache, &mut env, fn_cache);

                    // try to find if it was set on the cache
                    if var_cache.contains_key(name) {
                        *value_type = var_cache.get(name).cloned();
                        set_expression_type(value, value_type.as_ref().unwrap(), &mut var_cache);
                    } else {
                        // what here? no let type, no cache
                        println!("no cache let found")
                    }
                }
            }
            Statement::Mutate {
                name,
                value_type,
                value,
                ..
            } => {
                if let Some(value_type) = value_type {
                    // todo: check types matches?
                    var_cache.insert(name.clone(), value_type.clone());
                    set_expression_type(value, value_type, &mut var_cache);
                } else {
                    // evalue the value expr first to find a possible type.
                    if var_cache.contains_key(name) {
                        *value_type = var_cache.get(name).cloned();
                        let mut env = Some(value_type.clone().unwrap());
                        set_exp_types_from_cache(value, &mut var_cache, &mut env, fn_cache);
                    } else {
                        // no type info?
                    }
                }
            }
            Statement::If {
                condition,
                body,
                else_body,
            } => {
                let mut env = None;
                set_exp_types_from_cache(condition, &mut var_cache, &mut env, fn_cache);
                update_statements(body, &mut var_cache, fn_cache);
                if let Some(else_body) = else_body {
                    update_statements(else_body, &mut var_cache, fn_cache);
                }
            }
            Statement::Return(exp) => {
                if let Some(exp) = exp {
                    let mut env = None;
                    set_exp_types_from_cache(exp, &mut var_cache, &mut env, fn_cache);
                }
            }
            Statement::Function(_) => unreachable!(),
            Statement::Struct(_) => unreachable!(),
        }
    }
}

fn fn_return_type(func: &mut Function) -> Option<&mut Box<Expression>> {
    for st in func.body.iter_mut() {
        if let Statement::Return(r) = st {
            return r.as_mut();
        }
    }
    None
}

// set variables using the cache
fn set_exp_types_from_cache(
    exp: &mut Expression,
    var_cache: &mut HashMap<String, TypeExp>,
    env: &mut Option<TypeExp>,
    fn_cache: &HashMap<String, Function>,
) {
    match exp {
        Expression::Variable { name, value_type } => {
            let name = name.value.clone();
            if let Some(value_type) = value_type {
                // todo: check types matches?
                var_cache.insert(name, value_type.clone());
                *env = Some(value_type.clone());
            } else if var_cache.contains_key(&name) {
                *value_type = var_cache.get(&name).cloned();
                if env.is_none() {
                    *env = value_type.clone();
                }
            }
        }
        Expression::BinaryOp(lhs, op, rhs) => match op {
            ast::OpCode::Eq | ast::OpCode::Ne => {
                set_exp_types_from_cache(lhs, var_cache, env, fn_cache);
                set_exp_types_from_cache(rhs, var_cache, env, fn_cache);
                set_exp_types_from_cache(lhs, var_cache, env, fn_cache);
                *env = Some(TypeExp::Boolean);
            }
            _ => {
                set_exp_types_from_cache(lhs, var_cache, env, fn_cache);
                set_exp_types_from_cache(rhs, var_cache, env, fn_cache);
                set_exp_types_from_cache(lhs, var_cache, env, fn_cache); // needed in case 2 == x
            }
        },
        Expression::Literal(lit) => match lit {
            ast::LiteralValue::String(_) => {
                warn!("found string, unimplemented")
            }
            ast::LiteralValue::Integer { bits, signed, .. } => {
                if let Some(TypeExp::Integer {
                    bits: t_bits,
                    signed: t_signed,
                }) = env
                {
                    *bits = Some(*t_bits);
                    *signed = Some(*t_signed);
                }
            }
            ast::LiteralValue::Boolean(_) => {
                warn!("found bool, unimplemented")
            }
        },
        Expression::Call {
            function,
            args,
            value_type,
        } => {
            let fn_type = fn_cache.get(function).unwrap().clone();
            match value_type {
                Some(value_type) => *env = Some(value_type.clone()),
                None => {
                    if env.is_some() {
                        let env = env.clone();
                        *value_type = env.clone();
                    } else {
                        *value_type = fn_type.return_type.clone();
                        *env = fn_type.return_type.clone();
                    }
                }
            }

            for (i, arg) in args.iter_mut().enumerate() {
                let mut env = Some(fn_type.params[i].type_exp.clone());
                set_exp_types_from_cache(arg, var_cache, &mut env, fn_cache);
            }
        }
    }
}

fn set_expression_type(
    exp: &mut Expression,
    expected_type: &TypeExp,
    var_cache: &mut HashMap<String, TypeExp>,
) {
    match exp {
        Expression::Variable { name, value_type } => {
            // if needed?
            if value_type.is_none() {
                *value_type = Some(expected_type.clone());
            }
            if !var_cache.contains_key(&name.value) {
                var_cache.insert(name.value.clone(), expected_type.clone());
            }
        }
        Expression::BinaryOp(lhs, op, rhs) => match op {
            // ast::OpCode::Eq | ast::OpCode::Ne => {}
            _ => {
                set_expression_type(lhs, expected_type, var_cache);
                set_expression_type(rhs, expected_type, var_cache);
            }
        },
        Expression::Literal(lit) => match lit {
            ast::LiteralValue::String(_) => {
                warn!("found string, unimplemented")
            }
            ast::LiteralValue::Integer { bits, signed, .. } => {
                if let TypeExp::Integer {
                    bits: t_bits,
                    signed: t_signed,
                } = expected_type
                {
                    *bits = Some(*t_bits);
                    *signed = Some(*t_signed);
                }
            }
            ast::LiteralValue::Boolean(_) => {
                warn!("found bool, unimplemented")
            }
        },
        Expression::Call {
            function: _,
            args: _,
            value_type,
        } => {
            *value_type = Some(expected_type.clone());
        }
    }
}
