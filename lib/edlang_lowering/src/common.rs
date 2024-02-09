use std::collections::HashMap;

use edlang_ir::{Body, DefId, Local, ModuleBody, ProgramBody, Statement, TypeInfo, TypeKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct IdGenerator {
    pub current_id: usize,
    pub module_id: usize,
}

impl IdGenerator {
    pub const fn new(module_id: usize) -> Self {
        Self {
            current_id: 0,
            module_id,
        }
    }

    pub fn next_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }

    pub fn module_defid(&self) -> DefId {
        DefId {
            program_id: self.module_id,
            id: 0,
        }
    }

    pub fn next_defid(&mut self) -> DefId {
        let id = self.next_id();

        DefId {
            program_id: self.module_id,
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
    pub body: ProgramBody,
    pub unresolved_function_signatures:
        HashMap<DefId, (Vec<edlang_ast::Type>, Option<edlang_ast::Type>)>,
    pub gen: IdGenerator,
}

#[derive(Debug, Clone)]
pub struct BodyBuilder {
    pub local_module: DefId,
    pub body: Body,
    pub statements: Vec<Statement>,
    pub name_to_local: HashMap<String, usize>,
    pub ret_local: usize,
    pub ctx: BuildCtx,
}

impl BodyBuilder {
    pub fn add_local(&mut self, local: Local) -> usize {
        let id = self.body.locals.len();
        self.body.locals.push(local);
        id
    }

    pub fn _add_temp_local(&mut self, ty_kind: TypeKind) -> usize {
        let id = self.body.locals.len();
        self.body.locals.push(Local::temp(TypeInfo {
            span: None,
            kind: ty_kind,
        }));
        id
    }

    pub fn _get_local(&self, name: &str) -> Option<&Local> {
        self.body.locals.get(*(self.name_to_local.get(name)?))
    }

    pub fn get_module_body(&self) -> &ModuleBody {
        self.ctx.body.modules.get(&self.local_module).unwrap()
    }
}
