use std::collections::{HashMap, HashSet};

use crate::ast::{self, Expression, Function, Spanned, Statement, TypeExp};

#[derive(Debug, Clone)]
pub enum TypeError {
    Mismatch {
        found: TypeExp,
        expected: TypeExp,
        span: (usize, usize),
    },
    UndeclaredVariable {
        name: String,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, Default)]
struct Storage {
    structs: HashMap<String, HashMap<String, TypeExp>>,
    functions: HashMap<String, Function>,
}

type ScopeMap = HashMap<String, Vec<Option<TypeExp>>>;

// this works, but need to find a way to store the found info + handle literal integer types (or not?)
// maybe use scope ids
pub fn type_check(ast: &mut ast::Program) -> Result<(), TypeError> {
    let mut storage = Storage::default();

    // gather global constructs first
    for statement in ast.statements.iter_mut() {
        match &mut statement.value {
            Statement::Struct(st) => {
                let fields = st
                    .fields
                    .iter()
                    .map(|x| (x.ident.value.clone(), x.field_type.value.clone()))
                    .collect();
                storage.structs.insert(st.name.value.clone(), fields);
            }
            Statement::Function(function) => {
                storage
                    .functions
                    .insert(function.name.value.clone(), function.clone());
            }
            // todo: find globals here too
            _ => {}
        }
    }

    for statement in ast.statements.iter_mut() {
        if let Statement::Function(function) = &mut statement.value {
            let mut scope_vars: ScopeMap = HashMap::new();

            for arg in &function.params {
                scope_vars.insert(
                    arg.ident.value.clone(),
                    vec![Some(arg.type_exp.value.clone())],
                );
            }

            let func_info = function.clone();
            let (new_scope_vars, _) =
                type_inference_scope(&mut function.body, &scope_vars, &func_info, &storage)?;
            // todo: check all vars have  type info?
            function.scope_type_info = new_scope_vars
                .into_iter()
                .map(|(a, b)| (a, b.into_iter().map(Option::unwrap).collect()))
                .collect();
        }
    }
    Ok(())
}

/// Finds variable types in the scope, returns newly created variables to handle shadowing
fn type_inference_scope(
    statements: &mut [Spanned<ast::Statement>],
    scope_vars: &ScopeMap,
    func: &Function,
    storage: &Storage,
) -> Result<(ScopeMap, HashSet<String>), TypeError> {
    let mut scope_vars = scope_vars.clone();
    let mut new_vars: HashSet<String> = HashSet::new();

    for statement in statements {
        match &mut statement.value {
            Statement::Let {
                name,
                value,
                value_type,
            } => {
                new_vars.insert(name.value.clone());

                let exp_type = type_inference_expression(&value, &mut scope_vars, storage, None)?;

                if !scope_vars.contains_key(&name.value) {
                    scope_vars.insert(name.value.clone(), vec![]);
                }

                let var = scope_vars.get_mut(&name.value).unwrap();

                if value_type.is_none() {
                    var.push(exp_type);
                } else {
                    if exp_type.is_some() && exp_type != value_type.clone().map(|x| x.value) {
                        Err(TypeError::Mismatch {
                            found: exp_type.clone().unwrap(),
                            expected: value_type.clone().map(|x| x.value).unwrap(),
                            span: statement.span,
                        })?;
                    }
                    var.push(value_type.clone().map(|x| x.value));
                }
            }
            Statement::Mutate { name, value } => {
                if !scope_vars.contains_key(&name.value) {
                    Err(TypeError::UndeclaredVariable {
                        name: name.value.clone(),
                        span: name.span,
                    })?;
                }

                let exp_type = type_inference_expression(&value, &mut scope_vars, storage, None)?;
                let var = scope_vars.get_mut(&name.value).unwrap().last_mut().unwrap();

                if var.is_none() {
                    *var = exp_type;
                } else if exp_type.is_some() && &exp_type != var {
                    Err(TypeError::Mismatch {
                        found: exp_type.clone().unwrap(),
                        expected: var.clone().unwrap(),
                        span: statement.span,
                    })?;
                }
            }
            Statement::If {
                condition,
                body,
                else_body,
                scope_type_info,
                else_body_scope_type_info,
            } => {
                type_inference_expression(
                    &condition,
                    &mut scope_vars,
                    storage,
                    Some(TypeExp::Boolean),
                )?;

                let (new_scope_vars, new_vars) =
                    type_inference_scope(body, &scope_vars, func, storage)?;

                for (k, v) in new_scope_vars.iter() {
                    // not a new var within the scope (shadowing), so type info is valid
                    if scope_vars.contains_key(k) && !new_vars.contains(k) {
                        scope_vars.insert(k.clone(), v.clone());
                    }
                }

                *scope_type_info = new_scope_vars
                    .into_iter()
                    .map(|(a, b)| (a, b.into_iter().map(Option::unwrap).collect()))
                    .collect();

                if let Some(body) = else_body {
                    let (new_scope_vars, new_vars) =
                        type_inference_scope(body, &scope_vars, func, storage)?;

                    for (k, v) in new_scope_vars.iter() {
                        // not a new var within the scope (shadowing), so type info is valid
                        if scope_vars.contains_key(k) && !new_vars.contains(k) {
                            scope_vars.insert(k.clone(), v.clone());
                        }
                    }

                    *else_body_scope_type_info = new_scope_vars
                        .into_iter()
                        .map(|(a, b)| (a, b.into_iter().map(Option::unwrap).collect()))
                        .collect();
                }
            }
            Statement::Return(exp) => {
                if let Some(exp) = exp {
                    type_inference_expression(
                        exp,
                        &mut scope_vars,
                        storage,
                        func.return_type.clone().map(|x| x.value),
                    )?;
                }
            }
            Statement::Function(_) => unreachable!(),
            Statement::Struct(_) => unreachable!(),
        }
    }

    Ok((scope_vars, new_vars))
}

fn type_inference_expression(
    exp: &Spanned<Box<Expression>>,
    scope_vars: &mut ScopeMap,
    storage: &Storage,
    expected_type: Option<TypeExp>,
) -> Result<Option<TypeExp>, TypeError> {
    Ok(match &*exp.value {
        Expression::Literal(lit) => {
            match lit {
                ast::LiteralValue::String(_) => None, // todo
                ast::LiteralValue::Integer {
                    value: _,
                    bits,
                    signed,
                } => Some(TypeExp::Integer {
                    bits: *bits,
                    signed: *signed,
                }),
                ast::LiteralValue::Boolean(_) => Some(TypeExp::Boolean),
            }
        }
        Expression::Variable { name } => {
            let var = scope_vars
                .get_mut(name)
                .expect("to exist")
                .last_mut()
                .unwrap();

            if expected_type.is_some() {
                if var.is_none() {
                    *var = expected_type.clone();
                    expected_type
                } else if expected_type.is_some() {
                    if *var != expected_type {
                        Err(TypeError::Mismatch {
                            found: expected_type.clone().unwrap(),
                            expected: var.clone().unwrap(),
                            span: exp.span,
                        })?;
                    }
                    expected_type
                } else {
                    var.clone()
                }
            } else {
                var.clone()
            }
        }
        Expression::Call { function, args } => {
            let func = storage.functions.get(&function.value).cloned().unwrap();

            for (i, arg) in args.iter().enumerate() {
                let arg_type = func.params[i].type_exp.clone();
                // result is ignored, but need these to infer call arg types
                type_inference_expression(arg, scope_vars, storage, Some(arg_type.value))?;
            }

            func.return_type.map(|x| x.value)
        }
        Expression::BinaryOp(lhs, op, rhs) => match op {
            ast::OpCode::Eq | ast::OpCode::Ne => Some(TypeExp::Boolean),
            _ => {
                let lhs_type =
                    type_inference_expression(lhs, scope_vars, storage, expected_type.clone())?;
                let rhs_type = type_inference_expression(rhs, scope_vars, storage, expected_type)?;

                if lhs_type.is_some() && rhs_type.is_some() && lhs_type != rhs_type {
                    Err(TypeError::Mismatch {
                        found: rhs_type.clone().unwrap(),
                        expected: lhs_type.clone().unwrap(),
                        span: (0, 0), // todo
                    })?;
                }

                lhs_type.or(rhs_type)
            }
        },
    })
}
