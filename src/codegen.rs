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

use crate::ast::{self, Expression, Function, LiteralValue, OpCode, Statement, TypeExp};

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
    types: TypeStorage<'ctx>,
    struct_types: StructTypeStorage<'ctx>,
    // function to return type
    functions: HashMap<String, (Vec<TypeExp>, Option<TypeExp>)>,
    _program: ProgramData,
    ast: ast::Program,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub phi_counter: usize,
    pub type_exp: TypeExp,
}

pub type Variables<'ctx> = HashMap<String, Variable<'ctx>>;
pub type TypeStorage<'ctx> = HashMap<TypeExp, BasicTypeEnum<'ctx>>;

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
            types: HashMap::new(),
            struct_types: HashMap::new(),
            functions: HashMap::new(),
        };

        Ok(codegen)
    }

    pub fn compile_ast(&mut self) -> Result<()> {
        let mut functions = vec![];
        let mut func_info = HashMap::new();
        let mut types: TypeStorage<'ctx> = HashMap::new();
        let mut struct_types: StructTypeStorage<'ctx> = HashMap::new();

        // todo fix the grammar so top level statements are only functions and static vars.

        // create types
        for statement in &self.ast.statements {
            if let Statement::Struct(s) = &statement {
                let mut fields = HashMap::new();
                let mut field_types = vec![];

                for (i, field) in s.fields.iter().enumerate() {
                    if !types.contains_key(&field.type_exp) {
                        types.insert(field.type_exp.clone(), self.get_llvm_type(&field.type_exp)?);
                    }
                    let ty = self.get_llvm_type(&field.type_exp)?;
                    field_types.push(ty);
                    // todo: ensure alignment and padding here
                    fields.insert(field.ident.clone(), (i, field.type_exp.clone()));
                }

                let ty = self.context.struct_type(&field_types, false);

                let struct_type = StructTypeInfo { fields, ty };
                struct_types.insert(s.name.clone(), struct_type);
            }
        }

        self.struct_types = struct_types;

        // create the llvm functions first.
        for statement in &self.ast.statements {
            if let Statement::Function(function) = &statement {
                functions.push(function);
                let (args, ret_type) = self.compile_function_signature(function)?;
                let mut arg_types = vec![];
                for arg in args {
                    if !types.contains_key(&arg) {
                        let ty = self.get_llvm_type(&arg)?;
                        types.insert(arg.clone(), ty);
                    }
                    arg_types.push(arg);
                }
                if let Some(ret_type) = ret_type {
                    let ret_type = if !types.contains_key(&ret_type) {
                        let ty = self.get_llvm_type(&ret_type)?;
                        types.insert(ret_type.clone(), ty);
                        ret_type
                    } else {
                        ret_type
                    };
                    func_info.insert(function.name.clone(), (arg_types, Some(ret_type)));
                } else {
                    func_info.insert(function.name.clone(), (arg_types, None));
                }
            }
        }

        self.types = types;
        self.functions = func_info;

        info!("functions:\n{:#?}", self.functions);

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

    fn get_llvm_type(&self, id: &TypeExp) -> Result<BasicTypeEnum<'ctx>> {
        if let Some(ty) = self.types.get(id) {
            Ok(*ty)
        } else {
            Ok(match id {
                TypeExp::Integer { bits, signed: _ } => self
                    .context
                    .custom_width_int_type(*bits)
                    .as_basic_type_enum(),
                TypeExp::Boolean => self.context.bool_type().as_basic_type_enum(),
                TypeExp::Array { of, len } => {
                    let ty = self.get_llvm_type(of)?;
                    ty.array_type(len.unwrap()).as_basic_type_enum()
                }
                TypeExp::Pointer { target } => {
                    let ty = self.get_llvm_type(target)?;
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
    }

    /// creates the llvm function without the body, so other function bodies can call it.
    fn compile_function_signature(
        &self,
        function: &Function,
    ) -> Result<(Vec<TypeExp>, Option<TypeExp>)> {
        let args_types: Vec<BasicTypeEnum<'ctx>> = function
            .params
            .iter()
            .map(|param| &param.type_exp)
            .map(|t| self.get_llvm_type(t))
            .try_collect()?;

        let args_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            args_types.into_iter().map(|t| t.into()).collect_vec();

        let (fn_type, ret_type) = match &function.return_type {
            Some(id) => {
                let return_type = self.get_llvm_type(id)?;
                (return_type.fn_type(&args_types, false), Some(id.clone()))
            }
            None => (self.context.void_type().fn_type(&args_types, false), None),
        };

        self.module.add_function(&function.name, fn_type, None);

        Ok((
            function
                .params
                .iter()
                .map(|param| param.type_exp.clone())
                .collect(),
            ret_type,
        ))
    }

    fn compile_function(&self, function: &Function) -> Result<()> {
        let func = self.module.get_function(&function.name).unwrap();
        let entry_block = self.context.append_basic_block(func, "entry");

        self.builder.position_at_end(entry_block);

        let mut variables: Variables = HashMap::new();
        let mut types: TypeStorage = self.types.clone();

        for (i, param) in function.params.iter().enumerate() {
            let id = &param.ident;
            let param_value = func
                .get_nth_param(i.try_into().unwrap())
                .expect("parameter");
            variables.insert(
                id.clone(),
                Variable {
                    value: param_value,
                    phi_counter: 0,
                    type_exp: param.type_exp.clone(),
                },
            );
        }

        let mut has_return = false;

        for statement in &function.body {
            if let Statement::Return(_) = statement {
                has_return = true
            }
            self.compile_statement(func, statement, &mut variables, &mut types)?;
        }

        if !has_return {
            self.builder.build_return(None);
        }

        Ok(())
    }

    fn compile_statement(
        &self,
        function_value: FunctionValue,
        statement: &Statement,
        // value, assignments
        variables: &mut Variables<'ctx>,
        types: &mut TypeStorage<'ctx>,
    ) -> Result<()> {
        match statement {
            // Variable assignment
            Statement::Let {
                name,
                value,
                value_type: _,
                ..
            } => {
                let (value, value_type) = self
                    .compile_expression(value, variables, types)?
                    .expect("should have result");

                if !types.contains_key(&value_type) {
                    let ty = self.get_llvm_type(&value_type)?;
                    types.insert(value_type.clone(), ty);
                }

                info!("adding variable: name={}, ty={:?}", name, value_type);

                variables.insert(
                    name.clone(),
                    Variable {
                        value,
                        phi_counter: 0,
                        type_exp: value_type,
                    },
                );
            }
            Statement::Mutate { name, value, .. } => {
                let (value, value_type) = self
                    .compile_expression(value, variables, types)?
                    .expect("should have result");

                let var = variables.get_mut(name).expect("variable should exist");
                var.phi_counter += 1;
                var.value = value;
                assert_eq!(var.type_exp, value_type, "variable type shouldn't change!");
                info!("mutated variable: name={}, ty={:?}", name, var.type_exp);
            }
            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    let (value, _value_type) = self
                        .compile_expression(ret, variables, types)?
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
            } => {
                let (condition, _cond_type) = self
                    .compile_expression(condition, variables, types)?
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
                    self.compile_statement(function_value, s, &mut variables_if, types)?;
                }
                self.builder.build_unconditional_branch(merge_block);
                if_block = self.builder.get_insert_block().unwrap(); // update for phi

                let mut variables_else = variables.clone();
                if let Some(else_body) = else_body {
                    self.builder.position_at_end(else_block);

                    for s in else_body {
                        self.compile_statement(function_value, s, &mut variables_else, types)?;
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
                            processed_vars.insert(name, (phi, new_var.type_exp));
                        }
                    }
                }

                if else_body.is_some() {
                    for (name, new_var) in variables_else {
                        if variables.contains_key(&name) {
                            let old_var = variables.get(&name).unwrap();
                            if new_var.phi_counter > old_var.phi_counter {
                                if let Some((phi, _)) = processed_vars.get(&name) {
                                    phi.add_incoming(&[(&new_var.value, else_block)]);
                                } else {
                                    let phi = self.builder.build_phi(
                                        old_var.value.get_type(),
                                        &format!("{name}_phi"),
                                    );
                                    phi.add_incoming(&[(&old_var.value, else_block)]);
                                    processed_vars.insert(name, (phi, new_var.type_exp));
                                }
                            }
                        }
                    }
                }

                for (name, (phi, type_exp)) in processed_vars {
                    variables.insert(
                        name,
                        Variable {
                            value: phi.as_basic_value(),
                            phi_counter: 0,
                            type_exp,
                        },
                    );
                }
            }
            Statement::Function(_) => unreachable!(),
            Statement::Struct(_) => unreachable!(),
        };

        Ok(())
    }

    pub fn compile_expression(
        &self,
        expr: &Expression,
        variables: &mut Variables<'ctx>,
        types: &mut TypeStorage<'ctx>,
    ) -> Result<Option<(BasicValueEnum<'ctx>, TypeExp)>> {
        Ok(match expr {
            Expression::Variable {
                name,
                value_type: _,
            } => Some(self.compile_variable(&name.value, variables)?),
            Expression::Literal(term) => Some(self.compile_literal(term)?),
            Expression::Call {
                function,
                args,
                value_type,
            } => self.compile_call(function, args, variables, types, value_type.clone())?,
            Expression::BinaryOp(lhs, op, rhs) => {
                Some(self.compile_binary_op(lhs, op, rhs, variables, types)?)
            }
        })
    }

    pub fn compile_call(
        &self,
        func_name: &str,
        args: &[Box<Expression>],
        variables: &mut Variables<'ctx>,
        types: &mut TypeStorage<'ctx>,
        value_type: Option<TypeExp>,
    ) -> Result<Option<(BasicValueEnum<'ctx>, TypeExp)>> {
        info!("compiling fn call: func_name={}", func_name);
        let function = self.module.get_function(func_name).expect("should exist");

        let mut value_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

        for arg in args.iter() {
            let (res, _res_type) = self
                .compile_expression(arg, variables, types)?
                .expect("should have result");
            value_args.push(res.into());
        }

        let result = self
            .builder
            .build_call(function, &value_args, &format!("{func_name}_call"))
            .try_as_basic_value();

        Ok(match result {
            Either::Left(val) => Some((val, value_type.unwrap())),
            Either::Right(_) => None,
        })
    }

    pub fn compile_binary_op(
        &self,
        lhs: &Expression,
        op: &OpCode,
        rhs: &Expression,
        variables: &mut Variables<'ctx>,
        types: &mut TypeStorage<'ctx>,
    ) -> Result<(BasicValueEnum<'ctx>, TypeExp)> {
        let (lhs, lhs_type) = self
            .compile_expression(lhs, variables, types)?
            .expect("should have result");
        let (rhs, rhs_type) = self
            .compile_expression(rhs, variables, types)?
            .expect("should have result");

        assert_eq!(lhs_type, rhs_type);
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();

        let mut bool_result = false;
        let result = match op {
            OpCode::Add => self.builder.build_int_add(lhs, rhs, "add"),
            OpCode::Sub => self.builder.build_int_sub(lhs, rhs, "sub"),
            OpCode::Mul => self.builder.build_int_mul(lhs, rhs, "mul"),
            OpCode::Div => self.builder.build_int_signed_div(lhs, rhs, "div"),
            OpCode::Rem => self.builder.build_int_signed_rem(lhs, rhs, "rem"),
            OpCode::And => self.builder.build_and(lhs, rhs, "and"),
            OpCode::Or => self.builder.build_or(lhs, rhs, "or"),
            OpCode::Eq => {
                bool_result = true;
                self.builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
            }
            OpCode::Ne => {
                bool_result = true;
                self.builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "eq")
            }
        };

        let mut res_type = lhs_type;

        if bool_result {
            res_type = TypeExp::Integer {
                bits: 1,
                signed: false,
            };
        }

        Ok((result.as_basic_value_enum(), res_type))
    }

    pub fn compile_literal(&self, term: &LiteralValue) -> Result<(BasicValueEnum<'ctx>, TypeExp)> {
        let value = match term {
            LiteralValue::String(_s) => {
                todo!()
                /*
                self
                .context
                .const_string(s.as_bytes(), true)
                .as_basic_value_enum() */
            }
            LiteralValue::Boolean(v) => (
                self.context
                    .bool_type()
                    .const_int((*v).into(), false)
                    .as_basic_value_enum(),
                TypeExp::Boolean,
            ),
            LiteralValue::Integer {
                value,
                bits,
                signed,
            } => {
                let bits = bits.unwrap_or(32);
                let signed = signed.unwrap_or(true);
                (
                    self.context
                        .custom_width_int_type(bits)
                        .const_int(value.parse().unwrap(), false)
                        .as_basic_value_enum(),
                    TypeExp::Integer { bits, signed },
                )
            }
        };

        Ok(value)
    }

    pub fn compile_variable(
        &self,
        variable: &str,
        variables: &mut Variables<'ctx>,
    ) -> Result<(BasicValueEnum<'ctx>, TypeExp)> {
        let var = variables.get(variable).expect("value").clone();
        Ok((var.value, var.type_exp))
    }
}
