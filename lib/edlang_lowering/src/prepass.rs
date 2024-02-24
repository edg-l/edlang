use std::collections::HashMap;

use crate::{errors::LoweringError, DefId};

use super::common::BuildCtx;
use edlang_ast as ast;
use edlang_ir::ModuleBody;

pub fn prepass_module(mut ctx: BuildCtx, mod_def: &ast::Module) -> Result<BuildCtx, LoweringError> {
    let module_id = ctx.gen.next_defid();
    tracing::debug!("running ir prepass on module {:?}", module_id);

    ctx.body
        .top_level_module_names
        .insert(mod_def.name.name.clone(), module_id);
    ctx.body.top_level_modules.push(module_id);

    ctx.body.modules.insert(
        module_id,
        ModuleBody {
            module_id,
            parent_ids: vec![],
            name: mod_def.name.name.clone(),
            symbols: Default::default(),
            modules: Default::default(),
            functions: Default::default(),
            structs: Default::default(),
            types: Default::default(),
            constants: Default::default(),
            imports: Default::default(),
            span: mod_def.span,
        },
    );

    {
        let mut gen = ctx.gen;
        let current_module = ctx
            .body
            .modules
            .get_mut(&module_id)
            .expect("module should exist");

        for ct in &mod_def.contents {
            match ct {
                ast::ModuleStatement::Constant(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .constants
                        .insert(info.name.name.clone(), next_id);
                    current_module.constants.insert(next_id);
                }
                ast::ModuleStatement::Function(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .functions
                        .insert(info.name.name.clone(), next_id);
                    current_module.functions.insert(next_id);
                    ctx.unresolved_function_signatures.insert(
                        next_id,
                        (
                            info.params.iter().map(|x| &x.arg_type).cloned().collect(),
                            info.return_type.clone(),
                        ),
                    );
                }
                ast::ModuleStatement::Struct(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                    current_module.structs.insert(next_id);
                }
                /*
                ast::ModuleStatement::Type(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                    current_module.types.insert(next_id);
                }
                */
                ast::ModuleStatement::Module(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .modules
                        .insert(info.name.name.clone(), next_id);
                    current_module.modules.insert(next_id);
                }
            }
        }

        ctx.gen = gen;
    }

    for ct in &mod_def.contents {
        if let ast::ModuleStatement::Module(info) = ct {
            let current_module = ctx
                .body
                .modules
                .get_mut(&module_id)
                .expect("module should exist");

            let next_id = *current_module.symbols.modules.get(&info.name.name).unwrap();
            ctx = prepass_sub_module(ctx, &[module_id], next_id, info)?;
        }
    }

    Ok(ctx)
}

pub fn prepass_sub_module(
    mut ctx: BuildCtx,
    parent_ids: &[DefId],
    module_id: DefId,
    mod_def: &ast::Module,
) -> Result<BuildCtx, LoweringError> {
    tracing::debug!("running ir prepass on submodule {:?}", module_id);
    let mut submodule_parents_ids = parent_ids.to_vec();
    submodule_parents_ids.push(module_id);

    {
        let mut gen = ctx.gen;
        let mut submodule = ModuleBody {
            module_id,
            name: mod_def.name.name.clone(),
            parent_ids: parent_ids.to_vec(),
            imports: Default::default(),
            symbols: Default::default(),
            modules: Default::default(),
            functions: Default::default(),
            structs: Default::default(),
            types: Default::default(),
            constants: Default::default(),
            span: mod_def.span,
        };

        for ct in &mod_def.contents {
            match ct {
                ast::ModuleStatement::Constant(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .constants
                        .insert(info.name.name.clone(), next_id);
                    submodule.constants.insert(next_id);
                }
                ast::ModuleStatement::Function(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .functions
                        .insert(info.name.name.clone(), next_id);
                    submodule.functions.insert(next_id);
                    ctx.unresolved_function_signatures.insert(
                        next_id,
                        (
                            info.params.iter().map(|x| &x.arg_type).cloned().collect(),
                            info.return_type.clone(),
                        ),
                    );
                }
                ast::ModuleStatement::Struct(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                    submodule.structs.insert(next_id);
                }
                /*
                ast::ModuleStatement::Type(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                    submodule.types.insert(next_id);
                }
                 */
                ast::ModuleStatement::Module(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .modules
                        .insert(info.name.name.clone(), next_id);
                    submodule.modules.insert(next_id);
                }
            }
        }

        ctx.gen = gen;

        ctx.body.modules.insert(module_id, submodule);
    }

    for ct in &mod_def.contents {
        if let ast::ModuleStatement::Module(info) = ct {
            let next_id = ctx.gen.next_defid();
            ctx = prepass_sub_module(ctx, &submodule_parents_ids, next_id, info)?;
        }
    }

    Ok(ctx)
}

pub fn prepass_imports(
    mut ctx: BuildCtx,
    mod_def: &ast::Module,
) -> Result<BuildCtx, LoweringError> {
    let mod_id = *ctx
        .body
        .top_level_module_names
        .get(&mod_def.name.name)
        .unwrap();

    for import in &mod_def.imports {
        let imported_module_id = ctx
            .body
            .top_level_module_names
            .get(&import.module[0].name)
            .ok_or_else(|| LoweringError::ModuleNotFound {
                span: import.module[0].span,
                module: import.module[0].name.clone(),
            })?;
        let mut imported_module =
            ctx.body
                .modules
                .get(imported_module_id)
                .ok_or(LoweringError::IdNotFound {
                    span: mod_def.span,
                    id: *imported_module_id,
                })?;

        for module_path in import.module.iter().skip(1) {
            let imported_module_id = imported_module
                .symbols
                .modules
                .get(&module_path.name)
                .ok_or_else(|| LoweringError::ModuleNotFound {
                    span: module_path.span,
                    module: module_path.name.clone(),
                })?;
            imported_module = ctx.body.modules.get(imported_module_id).ok_or({
                LoweringError::IdNotFound {
                    span: module_path.span,
                    id: *imported_module_id,
                }
            })?;
        }

        let mut imports = HashMap::new();

        for sym in &import.symbols {
            if let Some(id) = imported_module.symbols.functions.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.structs.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.types.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.constants.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else {
                Err(LoweringError::ImportNotFound {
                    module_span: mod_def.span,
                    import_span: import.span,
                    symbol: sym.clone(),
                })?;
            }
        }

        ctx.body
            .modules
            .get_mut(&mod_id)
            .unwrap()
            .imports
            .extend(imports);
    }

    for c in &mod_def.contents {
        if let ast::ModuleStatement::Module(info) = c {
            ctx = prepass_imports_submodule(ctx, info, mod_id)?;
        }
    }

    Ok(ctx)
}

pub fn prepass_imports_submodule(
    mut ctx: BuildCtx,
    mod_def: &ast::Module,
    parent_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let mod_id = *ctx
        .body
        .modules
        .get(&parent_id)
        .unwrap()
        .symbols
        .modules
        .get(&mod_def.name.name)
        .ok_or_else(|| LoweringError::ModuleNotFound {
            span: mod_def.span,
            module: mod_def.name.name.clone(),
        })?;

    for import in &mod_def.imports {
        let imported_module_id = ctx
            .body
            .top_level_module_names
            .get(&import.module[0].name)
            .ok_or_else(|| LoweringError::ModuleNotFound {
                span: import.module[0].span,
                module: import.module[0].name.clone(),
            })?;
        let mut imported_module = ctx.body.modules.get(imported_module_id).ok_or_else(|| {
            LoweringError::ModuleNotFound {
                span: import.module[0].span,
                module: import.module[0].name.clone(),
            }
        })?;

        for module_path in import.module.iter().skip(1) {
            let imported_module_id = imported_module
                .symbols
                .modules
                .get(&module_path.name)
                .ok_or_else(|| LoweringError::ModuleNotFound {
                    span: module_path.span,
                    module: module_path.name.clone(),
                })?;
            imported_module = ctx.body.modules.get(imported_module_id).ok_or({
                LoweringError::IdNotFound {
                    span: import.span,
                    id: *imported_module_id,
                }
            })?;
        }

        let mut imports = HashMap::new();

        for sym in &import.symbols {
            if let Some(id) = imported_module.symbols.functions.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.structs.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.types.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.constants.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else {
                Err(LoweringError::ImportNotFound {
                    module_span: mod_def.span,
                    import_span: import.span,
                    symbol: sym.clone(),
                })?;
            }
        }

        ctx.body
            .modules
            .get_mut(&mod_id)
            .unwrap()
            .imports
            .extend(imports);
    }

    Ok(ctx)
}
