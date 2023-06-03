use std::collections::HashMap;

use tracing::{info, warn};

use crate::ast::{self, Expression, Function, Statement, TypeExp};

pub fn type_inference(ast: &mut ast::Program) {
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

            update_statements(&mut function.body, &mut var_cache);
        }
    }
}

fn update_statements(statements: &mut [Statement], var_cache: &mut HashMap<String, TypeExp>) {
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
                        set_exp_types_from_cache(value, &mut var_cache, &mut env);
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
                        set_exp_types_from_cache(value, &mut var_cache, &mut env);
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
                    set_exp_types_from_cache(value, &mut var_cache, &mut env);

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
                        set_exp_types_from_cache(value, &mut var_cache, &mut env);
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
                set_exp_types_from_cache(condition, &mut var_cache, &mut env);
                update_statements(body, &mut var_cache);
                if let Some(else_body) = else_body {
                    update_statements(else_body, &mut var_cache);
                }
            }
            Statement::Return(exp) => {
                if let Some(exp) = exp {
                    let mut env = None;
                    set_exp_types_from_cache(exp, &mut var_cache, &mut env);
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
                set_exp_types_from_cache(lhs, var_cache, env);
                set_exp_types_from_cache(rhs, var_cache, env);
                set_exp_types_from_cache(lhs, var_cache, env);
                *env = Some(TypeExp::Boolean);
            }
            _ => {
                set_exp_types_from_cache(lhs, var_cache, env);
                set_exp_types_from_cache(rhs, var_cache, env);
                set_exp_types_from_cache(lhs, var_cache, env); // needed in case 2 == x
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
            function: _,
            args: _,
            value_type,
        } => {
            match value_type {
                Some(value_type) => *env = Some(value_type.clone()),
                None => {
                    if env.is_some() {
                        *value_type = env.clone();
                    }
                }
            }
            // TODO: infer args based on function args!
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
