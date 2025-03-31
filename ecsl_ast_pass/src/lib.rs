#![feature(let_chains)]
use attributes::AttributeValidator;
use byt::BytecodeValidator;
use casing::CasingWarnings;
use definitions::TypeDefCollector;
use ecsl_ast::{
    data::{EnumDef, StructDef},
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

pub fn generate_pre_tyir(ty_ctxt: Arc<LocalTyCtxt>) {
    let mut scope = GenericsScope::new();
    for (_, def) in ty_ctxt.defined.read().unwrap().iter() {
        match def {
            Definition::Struct(StructDef {
                span,
                kind,
                ident,
                generics,
                attributes,
                ..
            })
            | Definition::Enum(EnumDef {
                span,
                kind,
                ident,
                generics,
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
                        unsafe { ty_ctxt.global.insert_tyir(tyid, tyir, *span, ty_ctxt.file) };
                        ty_ctxt.global.insert_size(tyid, builtin_size);
                        continue;
                    }
                }

                unsafe {
                    ty_ctxt.global.insert_tyir(
                        tyid,
                        TyIr::ADT(ecsl_ty::ADTDef {
                            id: tyid,
                            kind: *kind,
                            variant_hash: BTreeMap::new(),
                            variant_kinds: BTreeMap::new(),
                            resolved_generics: 0,
                            total_generics: generics,
                            attributes: attributes.clone(),
                        }),
                        *span,
                        ty_ctxt.file,
                    )
                };

                scope.pop();
            }
            _ => (),
        };
    }
}

pub fn generate_definition_tyir(ty_ctxt: Arc<LocalTyCtxt>) {
    mod ast {
        pub use ecsl_ast::callable::*;
        pub use ecsl_ast::data::*;
    }

    let parse_fields = |var: &mut ecsl_ty::VariantDef,
                        fields: &Vec<ast::FieldDef>,
                        scope: &GenericsScope| {
        for (i, f) in fields.iter().enumerate() {
            let id = FieldID::new(i);
            let symbol = ty_ctxt.table.get_symbol(f.ident).unwrap();
            if var.field_hash.insert(symbol.name, id).is_some() {
                ty_ctxt.diag.push_error(
                    EcslError::new(ErrorLevel::Error, "Duplicate field name").with_span(|_| f.span),
                );
            }

            let params =
                f.ty.generics
                    .params
                    .iter()
                    .map(|ty| ty_ctxt.get_tyid(ty, &scope).unwrap())
                    .collect();

            var.field_tys.insert(
                id,
                FieldDef {
                    id,
                    ty: ty_ctxt.get_tyid(&f.ty, &scope).unwrap(),
                    params,
                },
            );
        }
    };

    let mut scope = GenericsScope::new();
    for (_, def) in ty_ctxt.defined.read().unwrap().iter() {
        match def {
            Definition::Struct(ast::StructDef {
                span,
                ident,
                fields,
                generics,
                ..
            }) => {
                scope.add_opt(generics.clone());

                // Get TyID and TyIr
                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));
                let Some(mut tyir) = ty_ctxt.global.get_tyir(tyid).into_adt() else {
                    continue;
                };

                // Create a singular variant
                let mut variant = ecsl_ty::VariantDef {
                    id: VariantID::ZERO,
                    field_hash: BTreeMap::new(),
                    field_tys: BTreeMap::new(),
                };

                parse_fields(&mut variant, fields, &scope);

                // Insert the variant to the tyir and insert the tyir back into the ctxt
                tyir.variant_kinds.insert(VariantID::ZERO, variant);
                unsafe {
                    ty_ctxt
                        .global
                        .insert_tyir(tyid, TyIr::ADT(tyir), *span, ty_ctxt.file)
                };

                scope.pop();
            }
            Definition::Enum(ast::EnumDef {
                ident,
                generics,
                variants,
                span,
                ..
            }) => {
                scope.add_opt(generics.clone());

                // Get TyID and TyIr
                let tyid = ty_ctxt
                    .global
                    .get_or_create_tyid(GlobalID::new(*ident, ty_ctxt.file));
                let Some(mut tyir) = ty_ctxt.global.get_tyir(tyid).into_adt() else {
                    continue;
                };

                for (i, v) in variants.iter().enumerate() {
                    let var_id = VariantID::new(i);
                    let var_name = ty_ctxt.table.get_symbol(v.ident).unwrap();

                    if tyir.variant_hash.insert(var_name.name, var_id).is_some() {
                        ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Duplicate variant name")
                                .with_span(|_| v.span),
                        );
                    }

                    let mut variant = ecsl_ty::VariantDef::new(var_id);

                    parse_fields(&mut variant, &v.fields, &scope);
                    tyir.variant_kinds.insert(var_id, variant);
                }

                unsafe {
                    ty_ctxt
                        .global
                        .insert_tyir(tyid, TyIr::ADT(tyir), *span, ty_ctxt.file)
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
                attributes,
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

                    let params = ty
                        .generics
                        .params
                        .iter()
                        .map(|ty| ty_ctxt.get_tyid(ty, &scope).unwrap())
                        .collect();

                    let id = FieldID::new(i);
                    fn_params.insert(
                        id,
                        FieldDef {
                            id,
                            ty: ty_ctxt.get_tyid(&ty, &scope).unwrap(),
                            params,
                        },
                    );
                }

                let ret = match ret {
                    ast::RetTy::None(_) => FieldDef {
                        id: FieldID::ZERO,
                        ty: TyID::BOTTOM,
                        params: Vec::new(),
                    },
                    ast::RetTy::Ty(ty) => {
                        let params = ty
                            .generics
                            .params
                            .iter()
                            .map(|ty| ty_ctxt.get_tyid(ty, &scope).unwrap())
                            .collect();

                        FieldDef {
                            id: FieldID::ZERO,
                            ty: ty_ctxt.get_tyid(&ty, &scope).unwrap(),
                            params,
                        }
                    }
                };

                unsafe {
                    ty_ctxt.global.insert_tyir(
                        tyid,
                        TyIr::Fn(ecsl_ty::FnDef {
                            tyid,
                            kind: *kind,
                            params: fn_params,
                            ret,
                            resolved_generics: 0,
                            total_generics: generics,
                            attributes: attributes.clone(),
                        }),
                        *span,
                        ty_ctxt.file,
                    )
                };

                scope.pop();
            }
            Definition::AssocFunction(generics, ty, fn_def) => {
                todo!()
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

                debug!("{:?}", adt);

                let struct_fields = adt.get_struct_fields();
                for (i, f) in struct_fields.field_tys.iter() {
                    if let Some(field_adt) = ty_ctxt.global.get_tyir(f.ty).into_adt() {
                        debug!("inner {:?}", field_adt);

                        if f.params.len() != field_adt.total_generics {
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
                            if f.params.len() != field_adt.total_generics {
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
                    if let Some(param_adt) = ty_ctxt.global.get_tyir(p.ty).into_adt() {
                        if p.params.len() != param_adt.total_generics {
                            ty_ctxt.diag.push_error(
                                EcslError::new(ErrorLevel::Error, "Mismatched generics")
                                    .with_span(|_| params[fid.inner()].span),
                            );
                        }
                    }
                }
            }
            Definition::AssocFunction(generics, ty, fn_def) => {
                todo!()
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
