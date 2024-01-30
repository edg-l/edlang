use std::collections::HashMap;

use edlang_ast as ast;
use edlang_ir as ir;
use ir::{DefId, Local, Operand, Place, Statement, TypeInfo};

pub struct IdGenerator {
    pub current_id: usize,
    pub module_id: usize,
}

impl IdGenerator {
    pub const fn new(module_id: usize) -> Self {
        Self {
            current_id: 0,
            module_id: 0,
        }
    }

    pub fn next_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }

    pub fn next_defid(&mut self) -> DefId {
        let id = self.next_id();

        DefId {
            module_id: self.module_id,
            id,
        }
    }
}

pub fn lower_module(gen: &mut IdGenerator, module: &ast::Module) -> ir::ModuleBody {
    let mut body = ir::ModuleBody {
        module_id: gen.next_id(),
        functions: Default::default(),
        modules: Default::default(),
        span: module.span,
    };

    let mut module_gen_id = IdGenerator::new(body.module_id);

    for stmt in &module.contents {
        match stmt {
            ast::ModuleStatement::Function(func) => {
                let res = lower_function(&mut module_gen_id, func);
                body.functions.insert(res.def_id, res);
            }
            ast::ModuleStatement::Constant(_) => todo!(),
            ast::ModuleStatement::Struct(_) => todo!(),
            ast::ModuleStatement::Module(_) => todo!(),
        }
    }

    body
}

struct BodyBuilder {
    pub body: ir::Body,
    pub statements: Vec<ir::Statement>,
    pub locals: HashMap<String, usize>,
    pub ret_local: Option<usize>,
}

fn lower_function(gen: &mut IdGenerator, func: &ast::Function) -> ir::Body {
    let body = ir::Body {
        def_id: gen.next_defid(),
        ret_type: func.return_type.as_ref().map(|x| lower_type(gen, x)),
        locals: Default::default(),
        blocks: Default::default(),
        fn_span: func.span,
        is_pub: func.is_public,
        is_extern: func.is_extern,
    };

    let mut builder = BodyBuilder {
        body,
        statements: Vec::new(),
        locals: HashMap::new(),
        ret_local: None,
    };

    // store args ret

    if let Some(ret_type) = func.return_type.as_ref() {
        let ty = lower_type(gen, ret_type);

        let local = Local {
            mutable: false,
            span: None,
            ty,
            kind: ir::LocalKind::ReturnPointer,
        };

        builder.ret_local = Some(builder.body.locals.len());
        builder.body.locals.push(local);
    }

    for arg in &func.params {
        let ty = lower_type(gen, &arg.arg_type);
        let local = Local {
            mutable: false,
            span: Some(arg.span),
            ty,
            kind: ir::LocalKind::Arg,
        };
        builder
            .locals
            .insert(arg.name.name.clone(), builder.locals.len());
        builder.body.locals.push(local);
    }

    for stmt in &func.body.body {
        match stmt {
            ast::Statement::Let(info) => lower_let(gen, &mut builder, info),
            ast::Statement::Assign(_) => todo!(),
            ast::Statement::For(_) => todo!(),
            ast::Statement::While(_) => todo!(),
            ast::Statement::If(_) => todo!(),
            ast::Statement::Return(info) => lower_return(gen, &mut builder, info),
            ast::Statement::FnCall(_) => todo!(),
        }
    }

    builder.body
}

fn lower_let(gen: &mut IdGenerator, builder: &mut BodyBuilder, info: &ast::LetStmt) {
    let rvalue = lower_expr(builder, &info.value, Some(&lower_type(gen, &info.r#type)));

    let local = ir::Local {
        mutable: info.is_mut,
        span: Some(info.span),
        ty: lower_type(gen, &info.r#type),
        kind: ir::LocalKind::Temp,
    };

    let id = builder.body.locals.len();
    builder.locals.insert(info.name.name.clone(), id);
    builder.body.locals.push(local);

    builder.statements.push(ir::Statement {
        span: Some(info.span),
        kind: ir::StatementKind::StorageLive(id),
    });
    builder.statements.push(ir::Statement {
        span: Some(info.span),
        kind: ir::StatementKind::Assign(
            Place {
                local: id,
                projection: Default::default(),
            },
            rvalue,
        ),
    });
}

fn lower_expr(
    builder: &mut BodyBuilder,
    info: &ast::Expression,
    type_hint: Option<&TypeInfo>,
) -> ir::RValue {
    match info {
        ast::Expression::Value(info) => ir::RValue::Use(lower_value(builder, info, type_hint)),
        ast::Expression::FnCall(_) => todo!(),
        ast::Expression::Unary(_, _) => todo!(),
        ast::Expression::Binary(_, _, _) => todo!(),
    }
}

fn lower_value(
    builder: &mut BodyBuilder,
    info: &ast::ValueExpr,
    type_hint: Option<&TypeInfo>,
) -> Operand {
    match info {
        ast::ValueExpr::Bool { value, span } => ir::Operand::Constant(ir::ConstData {
            span: Some(*span),
            type_info: ir::TypeInfo {
                span: None,
                kind: ir::TypeKind::Bool,
            },
            kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::Bool(*value))),
        }),
        ast::ValueExpr::Char { value, span } => ir::Operand::Constant(ir::ConstData {
            span: Some(*span),
            type_info: ir::TypeInfo {
                span: None,
                kind: ir::TypeKind::Char,
            },
            kind: ir::ConstKind::Value(ir::ValueTree::Leaf(ir::ConstValue::U32((*value) as u32))),
        }),
        ast::ValueExpr::Int { value, span } => {
            let (ty, val) = match type_hint {
                Some(type_hint) => match &type_hint.kind {
                    ir::TypeKind::Int(type_hint) => match type_hint {
                        ir::IntTy::I128 => (
                            ir::TypeKind::Int(ir::IntTy::I128),
                            ir::ConstValue::I128((*value) as i128),
                        ),
                        ir::IntTy::I64 => (
                            ir::TypeKind::Int(ir::IntTy::I64),
                            ir::ConstValue::I64((*value) as i64),
                        ),
                        ir::IntTy::I32 => (
                            ir::TypeKind::Int(ir::IntTy::I32),
                            ir::ConstValue::I32((*value) as i32),
                        ),
                        ir::IntTy::I16 => (
                            ir::TypeKind::Int(ir::IntTy::I16),
                            ir::ConstValue::I16((*value) as i16),
                        ),
                        ir::IntTy::I8 => (
                            ir::TypeKind::Int(ir::IntTy::I8),
                            ir::ConstValue::I8((*value) as i8),
                        ),
                        ir::IntTy::Isize => todo!(),
                    },
                    ir::TypeKind::Uint(type_hint) => match type_hint {
                        ir::UintTy::U128 => (
                            ir::TypeKind::Uint(ir::UintTy::U128),
                            ir::ConstValue::U128(*value),
                        ),
                        ir::UintTy::U64 => (
                            ir::TypeKind::Uint(ir::UintTy::U64),
                            ir::ConstValue::U64((*value) as u64),
                        ),
                        ir::UintTy::U32 => (
                            ir::TypeKind::Uint(ir::UintTy::U32),
                            ir::ConstValue::U32((*value) as u32),
                        ),
                        ir::UintTy::U16 => (
                            ir::TypeKind::Uint(ir::UintTy::U16),
                            ir::ConstValue::U16((*value) as u16),
                        ),
                        ir::UintTy::U8 => (
                            ir::TypeKind::Uint(ir::UintTy::U8),
                            ir::ConstValue::U8((*value) as u8),
                        ),
                        _ => todo!(),
                    },
                    _ => unreachable!(),
                },
                None => todo!(),
            };

            ir::Operand::Constant(ir::ConstData {
                span: Some(*span),
                type_info: ir::TypeInfo {
                    span: None,
                    kind: ty,
                },
                kind: ir::ConstKind::Value(ir::ValueTree::Leaf(val)),
            })
        }
        ast::ValueExpr::Float { value, span } => todo!(),
        ast::ValueExpr::Str { value, span } => todo!(),
        ast::ValueExpr::Path(info) => {
            // add deref info to path
            Operand::Move(lower_path(builder, info))
        }
    }
}

fn lower_return(gen: &mut IdGenerator, builder: &mut BodyBuilder, info: &ast::ReturnStmt) {
    let ret_type = builder.body.ret_type.clone();
    if let Some(value) = &info.value {
        let rvalue = lower_expr(builder, value, ret_type.as_ref());
        let ret_local = builder.ret_local.unwrap();

        builder.statements.push(Statement {
            span: Some(info.span),
            kind: ir::StatementKind::Assign(
                Place {
                    local: ret_local,
                    projection: Default::default(),
                },
                rvalue,
            ),
        })
    }

    let statements = std::mem::take(&mut builder.statements);

    builder.body.blocks.push(ir::BasicBlock {
        id: builder.body.blocks.len(),
        statements: statements.into(),
        terminator: ir::Terminator::Return,
    });
}

fn lower_path(builder: &mut BodyBuilder, info: &ast::PathExpr) -> ir::Place {
    let local = *builder
        .locals
        .get(&info.first.name)
        .expect("local not found");

    Place {
        local,
        projection: Default::default(), // todo, field array deref
    }
}

pub fn lower_type(gen: &mut IdGenerator, t: &ast::Type) -> ir::TypeInfo {
    match t.name.name.as_str() {
        "u8" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U8),
        },
        "u16" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U16),
        },
        "u32" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U32),
        },
        "u64" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U64),
        },
        "u128" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Uint(ir::UintTy::U128),
        },
        "i8" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I8),
        },
        "i16" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I16),
        },
        "i32" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I32),
        },
        "i64" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I64),
        },
        "i128" => ir::TypeInfo {
            span: Some(t.span),
            kind: ir::TypeKind::Int(ir::IntTy::I128),
        },
        _ => todo!(),
    }
}
