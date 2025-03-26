#![feature(let_chains)]
use attributes::AttributeValidator;
use byt::BytecodeValidator;
use casing::CasingWarnings;
use definitions::TypeDefCollector;
use ecsl_ast::{
    item::{Item, ItemKind},
    parse::AttributeValue,
    visit::Visitor,
    SourceAST,
};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{FieldID, GlobalID, SourceFileID, TyID, VariantID};
use ecsl_parse::{source::SourceFile, table::SymbolTable, LexerTy};
use ecsl_ty::{
    def::Definition,
    import::{Import, MappedImport},
    local::LocalTyCtxt,
    FieldDef, GenericsScope, TyIr,
};
use entry_point::{EntryPoint, EntryPointError, EntryPointKind};
use fn_validator::FnValidator;
use import_collector::ImportCollector;
use log::debug;
use prelude::{rewrite_use_path, Prelude};
use std::{collections::BTreeMap, path::PathBuf, sync::Arc};

pub mod attributes;
pub mod byt;
pub mod casing;
pub mod definitions;
pub mod entry_point;
pub mod fn_validator;
pub mod import_collector;
pub mod prelude;

pub fn collect_prelude(ast: &SourceAST, id: SourceFileID) -> Prelude {
    let mut prelude = Prelude::new(id);
    prelude.visit_ast(&ast);
    prelude
}

pub fn include_prelude(
    prelude: &Prelude,
    ast: &mut SourceAST,
    table: Arc<SymbolTable>,
    lexer: &LexerTy,
) {
    let mut imports = prelude.imports.clone();

    for import in imports.iter_mut() {
        rewrite_use_path(&mut import.path, &table, lexer);
    }

    ast.items.extend(
        imports
            .drain(..)
            .map(|u| Item::new(u.span, ItemKind::Use(Box::new(u)))),
    );
}

pub fn validate_ast(ctxt: &Context, ast: &SourceAST, diag: DiagConn) {
    let mut fn_validator = FnValidator::new(diag.clone());
    fn_validator.visit_ast(ast);

    let mut attribute_validator = AttributeValidator::new(ctxt, diag.clone(), ast.file);
    attribute_validator.visit_ast(ast);

    let mut bytecode_validator = BytecodeValidator::new(ctxt, diag.clone(), ast.file);
    bytecode_validator.visit_ast(ast);
}

pub fn ast_definitions(ast: &SourceAST, ctxt: &Context, ty_ctxt: Arc<LocalTyCtxt>) {
    let mut definitions = TypeDefCollector::new(ty_ctxt.clone());
    definitions.visit_ast(ast);

    let mut imports = ImportCollector::new(ctxt, ty_ctxt.clone());
    imports.visit_ast(ast);
}

pub fn casing_warnings(ast: &SourceAST, diag: DiagConn, table: Arc<SymbolTable>) {
    let mut casing = CasingWarnings::new(diag, table);
    casing.visit_ast(&ast);
}

pub fn validate_imports(source: &SourceFile, ctxt: &Context, ty_ctxt: Arc<LocalTyCtxt>) {
    for (_, imported) in ty_ctxt.imported.write().unwrap().iter_mut() {
        let package = ctxt.get_source_file_package(source.id).unwrap();
        let Import::Unresolved(import) = imported else {
            panic!("{:?}", imported)
        };

        let first = import.path.iter().next().unwrap().to_str().unwrap();
        let imported_from = if let Some(cr) = package.get_dependency(first) {
            let mut components = import.path.components();
            let _ = components.next().unwrap();
            let import_path = components.collect::<PathBuf>();

            let source_file = match ctxt.get_source_file_from_package(&import_path, cr) {
                Ok(source_id) => source_id,
                Err(err) => {
                    ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, err).with_span(|_| import.span),
                    );
                    continue;
                }
            };

            source_file
        } else {
            let source_file = match ctxt.get_source_file_relative(&import.path, source.id) {
                Ok(source_id) => source_id,
                Err(err) => {
                    ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, err).with_span(|_| import.span),
                    );
                    continue;
                }
            };

            source_file
        };

        let sources = ty_ctxt.global.sources.read().unwrap();
        let import_source = sources.get(&imported_from).unwrap();

        let symbol_name = &ty_ctxt.table.get_symbol(import.from.symbol()).unwrap().name;
        let mapped_symbol = import_source.table.get_symbol_from_string(&symbol_name);

        *imported = if let Some(mapped_symbol) = mapped_symbol {
            Import::Resolved(MappedImport {
                from: import.from,
                to: GlobalID::new(mapped_symbol, imported_from),
            })
        } else {
            ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, &format!("Cannot find '{}'", symbol_name))
                    .with_span(|_| import.span),
            );
            Import::Unknown
        }
    }
}

pub fn generate_definition_tyir(ty_ctxt: Arc<LocalTyCtxt>) {
    mod ast {
        pub use ecsl_ast::callable::*;
        pub use ecsl_ast::data::*;
    }
    let mut scope = GenericsScope::new();
    for (_, def) in ty_ctxt.defined.read().unwrap().iter() {
        match def {
            Definition::Struct(ast::StructDef {
                span,
                kind,
                ident,
                generics,
                fields,
                attributes,
                ..
            }) => {
                let generics = scope.add_opt(generics.clone());

                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));

                if let Some(symbol) = ty_ctxt.table.get_symbol(*ident)
                    && let Some(builtin_size) = attributes.get_value(AttributeValue::Builtin)
                {
                    let tyir = match symbol.name.as_str() {
                        "int" => Some(TyIr::Int),
                        "float" => Some(TyIr::Float),
                        "bool" => Some(TyIr::Bool),
                        "char" => Some(TyIr::Char),
                        "str" => Some(TyIr::Str),
                        _ => None,
                    };

                    if let Some(tyir) = tyir {
                        unsafe { ty_ctxt.global.insert_tyir(tyid, tyir, *span) };
                        ty_ctxt.global.insert_size(tyid, builtin_size);
                        continue;
                    }
                }

                let mut field_hash = BTreeMap::new();
                let mut field_tys = BTreeMap::new();

                for (i, f) in fields.iter().enumerate() {
                    let id = FieldID::new(i);
                    let symbol = ty_ctxt.table.get_symbol(f.ident).unwrap();
                    if field_hash.insert(symbol.name, id).is_some() {
                        ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Duplicate field name")
                                .with_span(|_| f.span),
                        );
                    }

                    let params =
                        f.ty.generics
                            .as_ref()
                            .map(|g| {
                                g.params
                                    .iter()
                                    .map(|ty| ty_ctxt.get_tyid(ty, &scope))
                                    .collect()
                            })
                            .unwrap_or_default();
                    field_tys.insert(
                        id,
                        FieldDef {
                            id,
                            ty: ty_ctxt.get_tyid(&f.ty, &scope),
                            params,
                        },
                    );
                }

                unsafe {
                    ty_ctxt.global.insert_tyir(
                        tyid,
                        TyIr::ADT(ecsl_ty::ADTDef {
                            id: tyid,
                            kind: *kind,
                            variant_hash: BTreeMap::new(),
                            variant_kinds: vec![(
                                VariantID::ZERO,
                                ecsl_ty::VariantDef {
                                    id: VariantID::ZERO,
                                    field_hash,
                                    field_tys,
                                },
                            )]
                            .into_iter()
                            .collect(),
                            generics,
                        }),
                        *span,
                    )
                };

                scope.pop();
            }
            Definition::Enum(ast::EnumDef {
                kind,
                ident,
                generics,
                variants,
                span,
                ..
            }) => {
                let generics = scope.add_opt(generics.clone());

                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));

                let mut variant_hash = BTreeMap::new();
                let mut variant_kinds = BTreeMap::new();

                for (i, v) in variants.iter().enumerate() {
                    let var_id = VariantID::new(i);
                    let var_name = ty_ctxt.table.get_symbol(v.ident).unwrap();

                    if variant_hash.insert(var_name.name, var_id).is_some() {
                        ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Duplicate variant name")
                                .with_span(|_| v.span),
                        );
                    }

                    let mut field_hash = BTreeMap::new();
                    let mut field_tys = BTreeMap::new();

                    for (i, f) in v.fields.iter().enumerate() {
                        let id = FieldID::new(i);
                        let symbol = ty_ctxt.table.get_symbol(f.ident).unwrap();
                        if field_hash.insert(symbol.name, id).is_some() {
                            ty_ctxt.diag.push_error(
                                EcslError::new(ErrorLevel::Error, "Duplicate field name")
                                    .with_span(|_| f.span),
                            );
                        }

                        let params =
                            f.ty.generics
                                .as_ref()
                                .map(|g| {
                                    g.params
                                        .iter()
                                        .map(|ty| ty_ctxt.get_tyid(ty, &scope))
                                        .collect()
                                })
                                .unwrap_or_default();

                        field_tys.insert(
                            id,
                            FieldDef {
                                id,
                                ty: ty_ctxt.get_tyid(&f.ty, &scope),
                                params,
                            },
                        );
                    }

                    variant_kinds.insert(
                        var_id,
                        ecsl_ty::VariantDef {
                            id: var_id,
                            field_hash,
                            field_tys,
                        },
                    );
                }

                unsafe {
                    ty_ctxt.global.insert_tyir(
                        tyid,
                        TyIr::ADT(ecsl_ty::ADTDef {
                            id: tyid,
                            kind: *kind,
                            variant_hash,
                            variant_kinds,
                            generics,
                        }),
                        *span,
                    )
                };

                scope.pop();
            }
            Definition::Function(ast::FnDef {
                span,
                kind,
                ident,
                generics,
                params,
                ret,
                ..
            }) => {
                let generics = scope.add_opt(generics.clone());

                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));

                let mut fn_params = BTreeMap::new();
                for (i, p) in params.iter().enumerate() {
                    let ast::ParamKind::Normal(_, _, ty) = &p.kind else {
                        panic!()
                    };
                    let tyid = ty_ctxt.get_tyid(&ty, &scope);

                    let params = ty
                        .generics
                        .as_ref()
                        .map(|g| {
                            g.params
                                .iter()
                                .map(|ty| ty_ctxt.get_tyid(ty, &scope))
                                .collect()
                        })
                        .unwrap_or_default();

                    let id = FieldID::new(i);
                    fn_params.insert(
                        id,
                        FieldDef {
                            id,
                            ty: tyid,
                            params,
                        },
                    );
                }

                let ret = match ret {
                    ast::RetTy::None(_) => TyID::BOTTOM,
                    ast::RetTy::Ty(ty) => ty_ctxt.get_tyid(&ty, &scope),
                };

                unsafe {
                    ty_ctxt.global.insert_tyir(
                        tyid,
                        TyIr::Fn(ecsl_ty::FnDef {
                            tyid,
                            kind: *kind,
                            params: fn_params,
                            ret,
                            generics,
                        }),
                        *span,
                    )
                };

                scope.pop();
            }
        };
    }
}

pub fn validate_field_generics(ty_ctxt: Arc<LocalTyCtxt>) {
    mod ast {
        pub use ecsl_ast::callable::*;
        pub use ecsl_ast::data::*;
    }
    // let mut scope = GenericsScope::new();
    for (_, def) in ty_ctxt.defined.read().unwrap().iter() {
        match def {
            Definition::Struct(ast::StructDef { ident, fields, .. }) => {
                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));

                let Some(adt) = ty_ctxt.global.get_tyir(tyid).into_adt() else {
                    continue;
                };

                let struct_fields = adt.get_struct_fields();
                for (i, f) in struct_fields.field_tys.iter() {
                    if let Some(adt) = ty_ctxt.global.get_tyir(f.ty).into_adt() {
                        if f.params.len() != adt.generics.unwrap_or_default() {
                            ty_ctxt.diag.push_error(
                                EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                    .with_span(|_| fields.get(i.inner()).unwrap().span),
                            );
                        }
                    }
                }
            }
            Definition::Enum(ast::EnumDef {
                ident, variants, ..
            }) => {
                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));

                let Some(adt) = ty_ctxt.global.get_tyir(tyid).into_adt() else {
                    continue;
                };

                for (vid, var) in adt.variant_kinds.iter() {
                    for (fid, f) in var.field_tys.iter() {
                        if let Some(field_adt) = ty_ctxt.global.get_tyir(f.ty).into_adt() {
                            if f.params.len() != field_adt.generics.unwrap_or_default() {
                                ty_ctxt.diag.push_error(
                                    EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                        .with_span(|_| {
                                            variants[vid.inner()].fields[fid.inner()].span
                                        }),
                                );
                            }
                        }
                    }
                }
            }
            Definition::Function(ast::FnDef { ident, params, .. }) => {
                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));

                let fndef = ty_ctxt.global.get_tyir(tyid).into_fn().unwrap();

                for (fid, p) in fndef.params.iter() {
                    if let Some(adt) = ty_ctxt.global.get_tyir(p.ty).into_adt() {
                        if p.params.len() != adt.generics.unwrap_or_default() {
                            ty_ctxt.diag.push_error(
                                EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                    .with_span(|_| params[fid.inner()].span),
                            );
                        }
                    }
                }
            }
        };
    }
}

pub fn get_entry_point(
    ast: &SourceAST,
    ty_ctxt: Arc<LocalTyCtxt>,
) -> Option<(TyID, EntryPointKind)> {
    let mut entry_point = EntryPoint::new(ty_ctxt);
    entry_point.visit_ast(ast);

    if entry_point.entry_points.len() > 1 {
        entry_point.ty_ctxt.diag.push_error(EcslError::new(
            ErrorLevel::Error,
            EntryPointError::MultipleEntryPoints,
        ));
        None
    } else if entry_point.entry_points.len() == 0 {
        entry_point.ty_ctxt.diag.push_error(EcslError::new(
            ErrorLevel::Error,
            EntryPointError::NoEntryPoints,
        ));
        None
    } else {
        Some(entry_point.entry_points[0])
    }
}
