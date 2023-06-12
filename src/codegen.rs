use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    todo,
};

use color_eyre::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue},
    IntPredicate,
};
use itertools::{Either, Itertools};
use tracing::info;

use crate::ast::{self, Expression, Function, LiteralValue, OpCode, Spanned, Statement, TypeExp};

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
    //types: TypeStorage<'ctx>,
    struct_types: StructTypeStorage<'ctx>,
    // function to return type
    functions: HashMap<String, Function>,
    _program: ProgramData,
    ast: ast::Program,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub type_counter: usize,
    pub phi_counter: usize,
}

pub type Variables<'ctx> = HashMap<String, Variable<'ctx>>;
// pub type TypeStorage<'ctx> = HashMap<TypeExp, BasicTypeEnum<'ctx>>;

/// Holds the struct type and maps fields to types and the location within the struct.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructTypeInfo<'ctx> {
    ty: StructType<'ctx>,
    fields: HashMap<String, (usize, TypeExp)>,
}

type StructTypeStorage<'ctx> = HashMap<String, StructTypeInfo<'ctx>>;

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
            struct_types: HashMap::new(),
            functions: HashMap::new(),
        };

        Ok(codegen)
    }

    pub fn compile_ast(&mut self) -> Result<()> {
        let mut functions = HashMap::new();
        // let mut types: TypeStorage<'ctx> = HashMap::new();
        let mut struct_types: StructTypeStorage<'ctx> = HashMap::new();

        // todo fix the grammar so top level statements are only functions and static vars.

        // create types
        for statement in &self.ast.statements {
            if let Statement::Struct(s) = &statement.value {
                let mut fields = HashMap::new();
                let mut field_types = vec![];

                for (i, field) in s.fields.iter().enumerate() {
                    // todo: this doesnt handle out of order structs well
                    let ty = self.get_llvm_type(&field.field_type.value)?;
                    field_types.push(ty);
                    // todo: ensure alignment and padding here
                    fields.insert(
                        field.ident.value.clone(),
                        (i, field.field_type.value.clone()),
                    );
                }

                let ty = self.context.struct_type(&field_types, false);

                let struct_type = StructTypeInfo { fields, ty };
                struct_types.insert(s.name.value.clone(), struct_type);
            }
        }

        self.struct_types = struct_types;

        // create the llvm functions first.
        for statement in &self.ast.statements {
            if let Statement::Function(function) = &statement.value {
                functions.insert(function.name.value.clone(), function.clone());
                self.compile_function_signature(function)?;
            }
        }
        self.functions = functions;

        info!("functions:\n{:#?}", self.functions);

        // implement them.
        for (_, function) in &self.functions {
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

    fn get_llvm_type(&self, id: &TypeExp) -> Result<BasicTypeEnum<'ctx>> {
        Ok(match id {
            TypeExp::Integer { bits, signed: _ } => self
                .context
                .custom_width_int_type(*bits)
                .as_basic_type_enum(),
            TypeExp::Boolean => self.context.bool_type().as_basic_type_enum(),
            TypeExp::Array { of, len } => {
                let ty = self.get_llvm_type(&of.value)?;
                ty.array_type(len.unwrap()).as_basic_type_enum()
            }
            TypeExp::Pointer { target } => {
                let ty = self.get_llvm_type(&target.value)?;
                ty.ptr_type(Default::default()).as_basic_type_enum()
            }
            TypeExp::Other { id } => self
                .struct_types
                .get(id)
                .expect("struct type not found")
                .ty
                .as_basic_type_enum(),
        })
    }

    /// creates the llvm function without the body, so other function bodies can call it.
    fn compile_function_signature(&self, function: &Function) -> Result<()> {
        let args_types: Vec<BasicTypeEnum<'ctx>> = function
            .params
            .iter()
            .map(|param| &param.type_exp)
            .map(|t| self.get_llvm_type(&t.value))
            .try_collect()?;

        let args_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            args_types.into_iter().map(|t| t.into()).collect_vec();

        let fn_type = match &function.return_type {
            Some(id) => {
                let return_type = self.get_llvm_type(&id.value)?;
                return_type.fn_type(&args_types, false)
            }
            None => self.context.void_type().fn_type(&args_types, false),
        };

        self.module
            .add_function(&function.name.value, fn_type, None);

        Ok(())
    }

    fn compile_function(&self, function: &Function) -> Result<()> {
        let func = self.module.get_function(&function.name.value).unwrap();
        let entry_block = self.context.append_basic_block(func, "entry");

        self.builder.position_at_end(entry_block);

        let mut variables: Variables = HashMap::new();

        for (i, param) in function.params.iter().enumerate() {
            let id = &param.ident;
            let param_value = func
                .get_nth_param(i.try_into().unwrap())
                .expect("parameter");
            variables.insert(
                id.value.clone(),
                Variable {
                    value: param_value,
                    phi_counter: 0,
                    type_counter: 0,
                },
            );
        }

        let mut has_return = false;

        for statement in &function.body {
            if let Statement::Return(_) = statement.value {
                has_return = true
            }
            self.compile_statement(func, statement, &mut variables, &function.scope_type_info)?;
        }

        if !has_return {
            self.builder.build_return(None);
        }

        Ok(())
    }

    fn compile_statement(
        &self,
        function_value: FunctionValue,
        statement: &Spanned<Statement>,
        // value, assignments
        variables: &mut Variables<'ctx>,
        scope_info: &HashMap<String, Vec<TypeExp>>,
    ) -> Result<()> {
        match &statement.value {
            // Variable assignment
            Statement::Let {
                name,
                value,
                value_type: _,
                ..
            } => {
                let value = self
                    .compile_expression(&value, variables, scope_info)?
                    .expect("should have result");

                variables.insert(
                    name.value.clone(),
                    Variable {
                        value,
                        phi_counter: 0,
                        type_counter: 0,
                    },
                );
            }
            Statement::Mutate { name, value, .. } => {
                let value = self
                    .compile_expression(&value, variables, scope_info)?
                    .expect("should have result");

                let var = variables
                    .get_mut(&name.value)
                    .expect("variable should exist");
                var.phi_counter += 1;
                var.value = value;
            }
            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    let value = self
                        .compile_expression(&ret, variables, scope_info)?
                        .expect("should have result");
                    self.builder.build_return(Some(&value));
                } else {
                    self.builder.build_return(None);
                }
            }
            Statement::If {
                condition,
                body,
                else_body,
                scope_type_info,
                else_body_scope_type_info,
            } => {
                let condition = self
                    .compile_expression(condition, variables, scope_info)?
                    .expect("should produce a value");

                let mut if_block = self.context.append_basic_block(function_value, "if");
                let mut else_block = self.context.append_basic_block(function_value, "else");
                let merge_block = self.context.append_basic_block(function_value, "merge");

                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    if_block,
                    if else_body.is_some() {
                        else_block
                    } else {
                        merge_block
                    },
                );

                let mut variables_if = variables.clone();
                self.builder.position_at_end(if_block);
                for s in body {
                    self.compile_statement(function_value, s, &mut variables_if, scope_type_info)?;
                }
                self.builder.build_unconditional_branch(merge_block);
                if_block = self.builder.get_insert_block().unwrap(); // update for phi

                let mut variables_else = variables.clone();
                if let Some(else_body) = else_body {
                    self.builder.position_at_end(else_block);

                    for s in else_body {
                        self.compile_statement(
                            function_value,
                            s,
                            &mut variables_else,
                            else_body_scope_type_info,
                        )?;
                    }
                    self.builder.build_unconditional_branch(merge_block);
                    else_block = self.builder.get_insert_block().unwrap(); // update for phi
                }

                self.builder.position_at_end(merge_block);

                let mut processed_vars = HashMap::new();
                for (name, new_var) in variables_if {
                    if variables.contains_key(&name) {
                        let old_var = variables.get(&name).unwrap();
                        if new_var.phi_counter > old_var.phi_counter {
                            let phi = self
                                .builder
                                .build_phi(old_var.value.get_type(), &format!("{name}_phi"));
                            phi.add_incoming(&[(&new_var.value, if_block)]);
                            processed_vars.insert(name, phi);
                        }
                    }
                }

                if else_body.is_some() {
                    for (name, new_var) in variables_else {
                        if variables.contains_key(&name) {
                            let old_var = variables.get(&name).unwrap();
                            if new_var.phi_counter > old_var.phi_counter {
                                if let Some(phi) = processed_vars.get(&name) {
                                    phi.add_incoming(&[(&new_var.value, else_block)]);
                                } else {
                                    let phi = self.builder.build_phi(
                                        old_var.value.get_type(),
                                        &format!("{name}_phi"),
                                    );
                                    phi.add_incoming(&[(&old_var.value, else_block)]);
                                    processed_vars.insert(name, phi);
                                }
                            }
                        }
                    }
                }

                for (name, phi) in processed_vars {
                    /*
                    variables.insert(
                        name,
                        Variable {
                            value: phi.as_basic_value(),
                            phi_counter: 0,
                        },
                    );
                     */
                    let mut var = variables.get_mut(&name).unwrap();
                    var.value = phi.as_basic_value();
                }
            }
            Statement::Function(_) => unreachable!(),
            Statement::Struct(_) => unreachable!(),
        };

        Ok(())
    }

    pub fn compile_expression(
        &self,
        expr: &Spanned<Box<Expression>>,
        variables: &mut Variables<'ctx>,
        scope_info: &HashMap<String, Vec<TypeExp>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        Ok(match &*expr.value {
            Expression::Variable { name } => Some(self.compile_variable(&name, variables)?),
            Expression::Literal(term) => Some(self.compile_literal(term)?),
            Expression::Call { function, args } => {
                self.compile_call(function, args, variables, scope_info)?
            }
            Expression::BinaryOp(lhs, op, rhs) => {
                Some(self.compile_binary_op(lhs, op, rhs, variables, scope_info)?)
            }
        })
    }

    pub fn compile_call(
        &self,
        func_name: &Spanned<String>,
        args: &[Spanned<Box<Expression>>],
        variables: &mut Variables<'ctx>,
        scope_info: &HashMap<String, Vec<TypeExp>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        info!("compiling fn call: func_name={}", func_name.value);
        let function = self
            .module
            .get_function(&func_name.value)
            .expect("should exist");

        let mut value_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

        for arg in args.iter() {
            let res = self
                .compile_expression(arg, variables, scope_info)?
                .expect("should have result");
            value_args.push(res.into());
        }

        let result = self
            .builder
            .build_call(function, &value_args, &format!("{}_call", func_name.value))
            .try_as_basic_value();

        Ok(match result {
            Either::Left(val) => Some(val),
            Either::Right(_) => None,
        })
    }

    pub fn compile_binary_op(
        &self,
        lhs: &Spanned<Box<Expression>>,
        op: &OpCode,
        rhs: &Spanned<Box<Expression>>,
        variables: &mut Variables<'ctx>,
        scope_info: &HashMap<String, Vec<TypeExp>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let lhs = self
            .compile_expression(lhs, variables, scope_info)?
            .expect("should have result");
        let rhs = self
            .compile_expression(rhs, variables, scope_info)?
            .expect("should have result");

        assert_eq!(lhs.get_type(), rhs.get_type(), "type mismatch");

        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();

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
            LiteralValue::String(_s) => {
                todo!()
                /*
                self
                .context
                .const_string(s.as_bytes(), true)
                .as_basic_value_enum() */
            }
            LiteralValue::Boolean(v) => self
                .context
                .bool_type()
                .const_int((*v).into(), false)
                .as_basic_value_enum(),
            LiteralValue::Integer {
                value,
                bits,
                signed: _,
            } => {
                let bits = *bits;

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
        variables: &mut Variables<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = variables.get(variable).expect("value").clone();
        Ok(var.value)
    }
}
