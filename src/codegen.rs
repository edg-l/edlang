use std::collections::HashMap;

use color_eyre::Result;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};
use itertools::{Either, Itertools};

use statement::Statement;

use crate::ast::{self, statement, Expression, Function, Identifier, OpCode, SpanValue, Term};

#[derive(Debug, Clone)]
pub struct ProgramData {
    pub filename: String,
    pub source: String,
}

impl ProgramData {
    pub fn new(filename: &str, source: &str) -> Self {
        Self {
            filename: filename.to_string(),
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
            match &statement.value {
                Statement::Assignment(_) => unreachable!(),
                Statement::Definition(_) => todo!(),
                Statement::Return(_) => unreachable!(),
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
            .map(|param| param.type_name.0.value.as_str())
            .map(|t| self.get_llvm_type(t))
            .try_collect()?;

        let args_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            args_types.into_iter().map(|t| t.into()).collect_vec();

        let fn_type = match &function.return_type {
            Some(id) => self.get_llvm_type(&id.0.value)?.fn_type(&args_types, false),
            None => self.context.void_type().fn_type(&args_types, false),
        };

        self.module
            .add_function(&function.ident.0.value, fn_type, None);

        Ok(())
    }

    fn compile_function(&self, function: &Function) -> Result<()> {
        let func = self.module.get_function(&function.ident.0.value).unwrap();
        let entry_block = self.context.append_basic_block(func, "entry");

        self.builder.position_at_end(entry_block);

        let mut variables: HashMap<String, BasicValueEnum<'ctx>> = HashMap::new();

        for (i, param) in function.params.iter().enumerate() {
            let id = param.ident.clone();
            variables.insert(
                id.0.value.clone(),
                func.get_nth_param(i.try_into().unwrap())
                    .expect("parameter"),
            );
        }

        // todo: check function has return?
        let mut has_return = false;

        for statement in &function.body {
            if let Statement::Return(_) = statement.value {
                has_return = true
            }
            self.compile_statement(&entry_block, statement, &mut variables)?;
        }

        if !has_return {
            self.builder.build_return(None);
        }

        Ok(())
    }

    fn compile_statement(
        &self,
        block: &BasicBlock,
        statement: &SpanValue<Statement>,
        variables: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<()> {
        match &statement.value {
            // Variable assignment
            Statement::Assignment(body) => {
                let result = self
                    .compile_expression(block, &body.expr, variables)?
                    .expect("should have result");

                variables.insert(body.ident.0.value.clone(), result);
            }
            Statement::Definition(body) => {
                let result = self
                    .compile_expression(block, &body.expr, variables)?
                    .expect("should have result");

                variables.insert(body.ident.0.value.clone(), result);
            }
            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    let result = self
                        .compile_expression(block, ret, variables)?
                        .expect("should have result");
                    self.builder.build_return(Some(&result));
                } else {
                    self.builder.build_return(None);
                }
            }
            Statement::Function(_function) => unreachable!(),
        };

        Ok(())
    }

    pub fn compile_expression(
        &self,
        block: &BasicBlock,
        expr: &SpanValue<Box<Expression>>,
        variables: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        Ok(match &*expr.value {
            Expression::Term(term) => Some(self.compile_term(term, variables)?),
            Expression::Call(func_id, args) => self.compile_call(block, func_id, args, variables)?,
            Expression::Op(lhs, op, rhs) => Some(self.compile_op(block, lhs, op, rhs, variables)?),
        })
    }

    pub fn compile_call(
        &self,
        block: &BasicBlock,
        func_id: &Identifier,
        args: &[SpanValue<Box<Expression>>],
        variables: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        let func_name = &func_id.0.value;
        let function = self.module.get_function(func_name).expect("should exist");

        let mut value_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

        for arg in args {
            let res = self
                .compile_expression(block, arg, variables)?
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

    pub fn compile_op(
        &self,
        block: &BasicBlock,
        lhs: &SpanValue<Box<Expression>>,
        op: &OpCode,
        rhs: &SpanValue<Box<Expression>>,
        variables: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let lhs = self
            .compile_expression(block, lhs, variables)?
            .expect("should have result")
            .into_int_value();
        let rhs = self
            .compile_expression(block, rhs, variables)?
            .expect("should have result")
            .into_int_value();

        let result = match op {
            OpCode::Add => self.builder.build_int_add(lhs, rhs, "add"),
            OpCode::Sub => self.builder.build_int_sub(lhs, rhs, "sub"),
            OpCode::Mul => self.builder.build_int_mul(lhs, rhs, "mul"),
            OpCode::Div => self.builder.build_int_signed_div(lhs, rhs, "div"),
            OpCode::Rem => self.builder.build_int_signed_rem(lhs, rhs, "rem"),
        };

        Ok(result.as_basic_value_enum())
    }

    pub fn compile_term(
        &self,
        term: &Term,
        variables: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let value = match term {
            Term::Identifier(ident) => *variables.get(&ident.0.value).expect("value"),
            Term::Number(num) => self
                .context
                .i64_type()
                .const_int(num.0.value.try_into()?, true)
                .as_basic_value_enum(),
        };

        Ok(value)
    }
}
