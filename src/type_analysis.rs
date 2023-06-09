use std::collections::{HashMap, HashSet};

use crate::ast::{self, Expression, Function, Statement, TypeExp};

#[derive(Debug, Clone, Default)]
struct Storage {
    structs: HashMap<String, HashMap<String, TypeExp>>,
    functions: HashMap<String, Function>,
}

/*
To briefly summarize the union-find algorithm, given the set of all types in a proof,
it allows one to group them together into equivalence classes by means of a union procedure and to
 pick a representative for each such class using a find procedure. Emphasizing the word procedure in
 the sense of side effect, we're clearly leaving the realm of logic in order to prepare an effective algorithm.
  The representative of a u n i o n ( a , b ) {\mathtt {union}}(a,b) is determined such that, if both a and b are
  type variables then the representative is arbitrarily one of them, but while uniting a variable and a term, the
  term becomes the representative. Assuming an implementation of union-find at hand, one can formulate the unification of two monotypes as follows:
 */

// this works, but need to find a way to store the found info + handle literal integer types (or not?)
// maybe use scope ids
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
                } => Some(TypeExp::Integer {
                    bits: *bits,
                    signed: *signed,
                }),
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
