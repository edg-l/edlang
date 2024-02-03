use std::collections::HashMap;

use edlang_ir::{Body, DefId, Local, Statement, TypeInfo};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
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

    pub fn module_defid(&self) -> DefId {
        DefId {
            module_id: self.module_id,
            id: 0,
        }
    }

    pub fn next_defid(&mut self) -> DefId {
        let id = self.next_id();

        DefId {
            module_id: self.module_id,
            id,
        }
    }

    pub fn next_module_defid(&mut self) -> DefId {
        self.module_id += 1;
        self.current_id = 0;

        self.module_defid()
    }
}

#[derive(Debug, Clone, Default)]
pub struct BuildCtx {
    pub module_name_to_id: HashMap<String, DefId>,
    pub modules: HashMap<DefId, ModuleCtx>,
    pub functions: HashMap<DefId, Body>,
    pub gen: IdGenerator,
    pub symbol_names: HashMap<DefId, String>,
}

#[derive(Debug, Clone, Default)]
pub struct ModuleCtx {
    pub id: DefId,
    pub func_name_to_id: HashMap<String, DefId>,
    pub functions: HashMap<DefId, (Vec<TypeInfo>, TypeInfo)>,
    pub gen: IdGenerator,
}

#[derive(Debug, Clone)]
pub struct BodyBuilder {
    pub local_module: DefId,
    pub body: Body,
    pub statements: Vec<Statement>,
    pub locals: HashMap<String, usize>,
    pub ret_local: Option<usize>,
    pub ctx: BuildCtx,
}

impl BodyBuilder {
    pub fn add_local(&mut self, local: Local) -> usize {
        let id = self.body.locals.len();
        self.body.locals.push(local);
        id
    }
    pub fn get_local(&self, name: &str) -> Option<&Local> {
        self.body.locals.get(*(self.locals.get(name)?))
    }

    pub fn get_current_module(&self) -> &ModuleCtx {
        self.ctx
            .modules
            .get(&self.local_module)
            .expect("current module should exist")
    }

    pub fn get_current_module_mut(&mut self) -> &mut ModuleCtx {
        self.ctx
            .modules
            .get_mut(&self.local_module)
            .expect("current module should exist")
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<&(Vec<TypeInfo>, TypeInfo)> {
        let id = self.get_current_module().func_name_to_id.get(name)?;
        let f = self.get_current_module().functions.get(&id)?;
        Some(f)
    }
}
