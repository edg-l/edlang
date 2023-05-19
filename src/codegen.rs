use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    todo,
};

use color_eyre::Result;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    IntPredicate,
};
use itertools::{Either, Itertools};

use crate::ast::{self, Expression, Function, LiteralValue, OpCode, Statement};

#[derive(Debug, Clone)]
pub struct ProgramData {
    pub filename: PathBuf,
    pub source: String,
}

impl ProgramData {
    pub fn new(filename: &Path, source: &str) -> Self {
        Self {
            filename: filename.to_path_buf(),
            source: source.to_string(),
        }
    }
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    _program: ProgramData,
    ast: ast::Program,
}

#[derive(Debug, Clone)]
struct BlockInfo<'a> {
    pub blocks: Vec<BasicBlock<'a>>,
    pub current_block: usize,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        _program: ProgramData,
        ast: ast::Program,
    ) -> Result<Self> {
        let module = context.create_module(module_name);

        let codegen = CodeGen {
            context,
            module,
            builder: context.create_builder(),
            _program,
            ast,
        };

        Ok(codegen)
    }

    pub fn compile_ast(&self) -> Result<()> {
        let mut functions = vec![];

        // todo fix the grammar so top level statements are only functions and static vars.

        // create the llvm functions first.
        for statement in &self.ast.statements {
            match &statement {
                Statement::Variable { .. } => unreachable!(),
                Statement::Return(_) => unreachable!(),
                Statement::If { .. } => unreachable!(),
                Statement::Function(function) => {
                    functions.push(function);
                    self.compile_function_signature(function)?;
                }
            }
        }

        // implement them.
        for function in functions {
            self.compile_function(function)?;
        }

        Ok(())
    }

    pub fn generated_code(&self) -> String {
        if let Err(err) = self.module.verify() {
            eprintln!("error:\n{}", err);
        }
        self.module.print_to_string().to_str().unwrap().to_string()
    }

    fn get_llvm_type(&self, id: &str) -> Result<BasicTypeEnum<'ctx>> {
        Ok(match id {
            "i64" => self.context.i64_type().as_basic_type_enum(),
            "i32" => self.context.i32_type().as_basic_type_enum(),
            "i8" => self.context.i8_type().as_basic_type_enum(),
            "u8" => self.context.i8_type().as_basic_type_enum(),
            _ => todo!(),
        })
    }

    /// creates the llvm function without the body, so other function bodies can call it.
    fn compile_function_signature(&self, function: &Function) -> Result<()> {
        let args_types: Vec<BasicTypeEnum<'ctx>> = function
            .params
            .iter()
            .map(|param| param.type_name.as_str())
            .map(|t| self.get_llvm_type(t))
            .try_collect()?;

        let args_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            args_types.into_iter().map(|t| t.into()).collect_vec();

        let fn_type = match &function.return_type {
            Some(id) => self.get_llvm_type(id)?.fn_type(&args_types, false),
            None => self.context.void_type().fn_type(&args_types, false),
        };

        self.module.add_function(&function.name, fn_type, None);

        Ok(())
    }

    fn compile_function(&self, function: &Function) -> Result<()> {
        let func = self.module.get_function(&function.name).unwrap();
        let entry_block = self.context.append_basic_block(func, "entry");

        self.builder.position_at_end(entry_block);

        let mut variables: HashMap<String, (BasicValueEnum<'ctx>, usize)> = HashMap::new();

        for (i, param) in function.params.iter().enumerate() {
            let id = param.ident.clone();
            variables.insert(
                id.clone(),
                (
                    func.get_nth_param(i.try_into().unwrap())
                        .expect("parameter"),
                    0,
                ),
            );
        }

        let mut has_return = false;

        for statement in &function.body {
            if let Statement::Return(_) = statement {
                has_return = true
            }
            self.compile_statement(statement, &mut variables)?;
        }

        if !has_return {
            self.builder.build_return(None);
        }

        Ok(())
    }

    fn compile_statement(
        &self,
        statement: &Statement,
        // value, assignments
        variables: &mut HashMap<String, (BasicValueEnum<'ctx>, usize)>,
    ) -> Result<()> {
        match statement {
            // Variable assignment
            Statement::Variable { name, value } => {
                let result = self
                    .compile_expression(value, variables)?
                    .expect("should have result");

                let accesses = if let Some(x) = variables.get(name) {
                    x.1 + 1
                } else {
                    0
                };
                variables.insert(name.clone(), (result, accesses));
            }
            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    let result = self
                        .compile_expression(ret, variables)?
                        .expect("should have result");
                    self.builder.build_return(Some(&result));
                } else {
                    self.builder.build_return(None);
                }
            }
            Statement::If {
                condition,
                body,
                else_body,
            } => {
                let condition = self
                    .compile_expression(condition, variables)?
                    .expect("should produce a value");

                let func = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .expect("parent should exist");

                let mut if_block = self.context.append_basic_block(func, "if");
                let mut else_block = self.context.append_basic_block(func, "else");
                let merge_block = self.context.append_basic_block(func, "merge");

                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    if_block,
                    if let Some(else_body) = else_body {
                        else_block
                    } else {
                        merge_block
                    },
                );

                let mut variables_if = variables.clone();
                self.builder.position_at_end(if_block);
                for s in body {
                    self.compile_statement(s, &mut variables_if);
                }
                // should we set the builder at the end of the if_block again?
                self.builder.build_unconditional_branch(merge_block);
                if_block = self.builder.get_insert_block().unwrap(); // update for phi

                let mut variables_else = variables.clone();
                if let Some(else_body) = else_body {
                    self.builder.position_at_end(else_block);

                    for s in else_body {
                        self.compile_statement(s, &mut variables_else);
                    }
                    // should we set the builder at the end of the if_block again?
                    self.builder.build_unconditional_branch(merge_block);
                    else_block = self.builder.get_insert_block().unwrap(); // update for phi
                }

                self.builder.position_at_end(merge_block);

                let mut processed_vars = HashMap::new();
                for (name, (value, acc)) in variables_if {
                    if variables.contains_key(&name) {
                        let (old_val, old_acc) = variables.get(&name).unwrap();
                        if acc > *old_acc {
                            let phi = self
                                .builder
                                .build_phi(old_val.get_type(), &format!("{name}_phi"));
                            phi.add_incoming(&[(&value, if_block)]);
                            processed_vars.insert(name, (value, phi));
                        }
                    }
                }

                if else_body.is_some() {
                    for (name, (value, acc)) in variables_else {
                        if variables.contains_key(&name) {
                            let (old_val, old_acc) = variables.get(&name).unwrap();
                            if acc > *old_acc {
                                if let Some((_, phi)) = processed_vars.get(&name) {
                                    phi.add_incoming(&[(&value, else_block)]);
                                } else {
                                    let phi = self
                                        .builder
                                        .build_phi(old_val.get_type(), &format!("{name}_phi"));
                                    phi.add_incoming(&[(&value, else_block)]);
                                    processed_vars.insert(name, (value, phi));
                                }
                            }
                        }
                    }
                }

                for (name, (_, phi)) in processed_vars {
                    variables.insert(name, (phi.as_basic_value(), 0));
                }
            }
            Statement::Function(_function) => unreachable!(),
        };

        Ok(())
    }

    pub fn compile_expression(
        &self,
        expr: &Expression,
        variables: &mut HashMap<String, (BasicValueEnum<'ctx>, usize)>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        Ok(match expr {
            Expression::Variable(term) => Some(self.compile_variable(term, variables)?),
            Expression::Literal(term) => Some(self.compile_literal(term)?),
            Expression::Call { function, args } => self.compile_call(function, args, variables)?,
            Expression::BinaryOp(lhs, op, rhs) => {
                Some(self.compile_binary_op(lhs, op, rhs, variables)?)
            }
        })
    }

    pub fn compile_call(
        &self,
        func_name: &str,
        args: &[Box<Expression>],
        variables: &mut HashMap<String, (BasicValueEnum<'ctx>, usize)>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        let function = self.module.get_function(func_name).expect("should exist");

        let mut value_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

        for arg in args {
            let res = self
                .compile_expression(arg, variables)?
                .expect("should have result");
            value_args.push(res.into());
        }

        let result = self
            .builder
            .build_call(function, &value_args, &format!("{func_name}_call"))
            .try_as_basic_value();

        Ok(match result {
            Either::Left(val) => Some(val),
            Either::Right(_) => None,
        })
    }

    pub fn compile_binary_op(
        &self,
        lhs: &Expression,
        op: &OpCode,
        rhs: &Expression,
        variables: &mut HashMap<String, (BasicValueEnum<'ctx>, usize)>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let lhs = self
            .compile_expression(lhs, variables)?
            .expect("should have result")
            .into_int_value();
        let rhs = self
            .compile_expression(rhs, variables)?
            .expect("should have result")
            .into_int_value();

        let result = match op {
            OpCode::Add => self.builder.build_int_add(lhs, rhs, "add"),
            OpCode::Sub => self.builder.build_int_sub(lhs, rhs, "sub"),
            OpCode::Mul => self.builder.build_int_mul(lhs, rhs, "mul"),
            OpCode::Div => self.builder.build_int_signed_div(lhs, rhs, "div"),
            OpCode::Rem => self.builder.build_int_signed_rem(lhs, rhs, "rem"),
            OpCode::And => self.builder.build_and(lhs, rhs, "and"),
            OpCode::Or => self.builder.build_or(lhs, rhs, "or"),
            OpCode::Eq => self
                .builder
                .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq"),
            OpCode::Ne => self
                .builder
                .build_int_compare(IntPredicate::NE, lhs, rhs, "eq"),
        };

        Ok(result.as_basic_value_enum())
    }

    pub fn compile_literal(&self, term: &LiteralValue) -> Result<BasicValueEnum<'ctx>> {
        let value = match term {
            LiteralValue::String => todo!(),
            LiteralValue::Integer {
                bits,
                signed: _,
                value,
            } => {
                // todo: type resolution for bit size?
                let bits = bits.unwrap_or(32);

                self.context
                    .custom_width_int_type(bits)
                    .const_int(value.parse().unwrap(), false)
                    .as_basic_value_enum()
            }
        };

        Ok(value)
    }

    pub fn compile_variable(
        &self,
        variable: &str,
        variables: &mut HashMap<String, (BasicValueEnum<'ctx>, usize)>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = *variables.get(variable).expect("value");
        Ok(var.0)
    }
}
