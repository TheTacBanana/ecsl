#![feature(let_chains)]
use attributes::AttributeValidator;
use bimap::BiBTreeMap;
use byt::BytecodeValidator;
use casing::CasingWarnings;
use definitions::TypeDefCollector;
use ecsl_assembler::header::EntryPointKind;
use ecsl_ast::{
    data::{EnumDef, StructDef},
    item::{Item, ItemKind},
    parse::{AttributeValue, FnDef},
    visit::Visitor,
    SourceAST,
};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{FieldID, GlobalID, SourceFileID, TyID, VariantID};
use ecsl_parse::{source::SourceFile, table::SymbolTable, LexerTy};
use ecsl_ty::{
    def::Definition, import::Import, local::LocalTyCtxt, FieldDef, FnParent, GenericsScope, TyIr,
};
use entry_point::{EntryPoint, EntryPointError};
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
    let mut imported = ty_ctxt.imported.write().unwrap();
    let imports = std::mem::take(&mut (*imported));
    drop(imported);

    let mut converted_imports = BiBTreeMap::new();
    for (_, imported) in imports {
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

        let symbol_name = &ty_ctxt.table.get_symbol(import.from).unwrap().name;
        let mapped_symbol = import_source.table.get_symbol_from_string(&symbol_name);

        if let Some(mapped_symbol) = mapped_symbol {
            converted_imports.insert(
                (None, import.from),
                GlobalID::new(None, mapped_symbol, imported_from),
            );

            let assoc = import_source.assoc.read().unwrap();
            if let Some(assoc) = assoc.get(&mapped_symbol) {
                for (symbol, _) in assoc {
                    let imported_symbol = import_source.table.get_symbol(*symbol).unwrap();
                    let symbol_name = &ty_ctxt.table.create_entry(imported_symbol.name);

                    converted_imports.insert(
                        (Some(import.from), *symbol_name),
                        GlobalID::new(Some(mapped_symbol), *symbol, imported_from),
                    );
                }
            }
        } else {
            ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, &format!("Cannot find '{}'", symbol_name))
                    .with_span(|_| import.span),
            );
        }
    }
    let mut imported_resolved = ty_ctxt.imported_resolved.write().unwrap();
    imported_resolved.extend(converted_imports);
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
                scope.add_opt(generics.clone());

                let tyid =
                    ty_ctxt
                        .global
                        .get_or_create_tyid(GlobalID::new(None, *ident, ty_ctxt.file));

                if let Some(symbol) = ty_ctxt.table.get_symbol(*ident)
                    && let Some(builtin_size) = attributes.get_value(AttributeValue::Builtin)
                {
                    let tyir = match symbol.name.as_str() {
                        "int" => Some(TyIr::Int),
                        "float" => Some(TyIr::Float),
                        "bool" => Some(TyIr::Bool),
                        "char" => Some(TyIr::Char),
                        "str" => Some(TyIr::Str),
                        "Entity" => Some(TyIr::Entity),
                        "Query" => Some(TyIr::Query),
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
                            total_generics: scope.total(),
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

    let parse_function =
        |tyid: TyID, parent: Option<FieldDef>, fn_def: &ast::FnDef, scope: &mut GenericsScope| {
            let FnDef {
                span,
                kind,
                attributes,
                generics,
                params,
                ret,
                ..
            } = fn_def;

            scope.add_opt(generics.clone());

            let mut fn_parent = FnParent::None;
            let mut fn_params = BTreeMap::new();
            for (i, p) in params.iter().enumerate() {
                let id = FieldID::new(i);
                let field_def = match &p.kind {
                    ast::ParamKind::Normal(_, _, ty) => {
                        let params = ty
                            .generics
                            .params
                            .iter()
                            .map(|ty| ty_ctxt.get_tyid(ty, &scope).unwrap())
                            .collect();

                        FieldDef {
                            id,
                            ty: ty_ctxt.get_tyid(&ty, &scope).unwrap(),
                            params,
                        }
                    }
                    ast::ParamKind::SelfReference(mutable, _) => {
                        let parent = parent.clone().unwrap();

                        fn_parent = FnParent::Ref(*mutable, parent.clone());

                        FieldDef {
                            id,
                            ty: ty_ctxt.global.tyid_from_tyir(TyIr::Ref(*mutable, parent)),
                            params: Vec::new(),
                        }
                    }
                    ast::ParamKind::SelfValue(mutable, _) => {
                        fn_parent = FnParent::Value(*mutable, parent.clone().unwrap());

                        parent.clone().unwrap()
                    }
                };

                fn_params.insert(id, field_def);
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
                        parent: fn_parent,
                        tyid,
                        kind: *kind,
                        params: fn_params,
                        ret,
                        resolved_generics: 0,
                        total_generics: scope.total(),
                        attributes: attributes.clone(),
                    }),
                    *span,
                    ty_ctxt.file,
                )
            };

            scope.pop();
        };

    let process_def = |def: &Definition, scope: &mut GenericsScope| {
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
                let tyid =
                    ty_ctxt
                        .global
                        .get_or_create_tyid(GlobalID::new(None, *ident, ty_ctxt.file));
                let Some(mut tyir) = ty_ctxt.global.get_tyir(tyid).into_adt() else {
                    return;
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
                let tyid =
                    ty_ctxt
                        .global
                        .get_or_create_tyid(GlobalID::new(None, *ident, ty_ctxt.file));
                let Some(mut tyir) = ty_ctxt.global.get_tyir(tyid).into_adt() else {
                    return;
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
            Definition::Function(f) => {
                let tyid =
                    ty_ctxt
                        .global
                        .get_or_create_tyid(GlobalID::new(None, f.ident, ty_ctxt.file));

                parse_function(tyid, None, f, scope);
            }
            Definition::AssocFunction(generics, ty, f) => {
                scope.add_opt(generics.clone());

                let Some(impl_id) = ty_ctxt.get_tyid(ty, &scope) else {
                    ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, "Cannot find type for impl block")
                            .with_span(|_| ty.span),
                    );
                    return;
                };
                let impl_tyir = ty_ctxt.global.get_tyir(impl_id);
                debug!("{:?}", impl_tyir);

                let Some(fn_scope) = ty.into_scope() else {
                    ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, "Unknown Type").with_span(|_| f.span),
                    );
                    return;
                };

                let fnid = ty_ctxt.global.get_or_create_tyid(GlobalID::new(
                    Some(fn_scope),
                    f.ident,
                    ty_ctxt.file,
                ));

                match impl_tyir {
                    TyIr::Unknown => {
                        ty_ctxt.diag.push_error(
                            EcslError::new(ErrorLevel::Error, "Cannot find type for impl block")
                                .with_span(|_| ty.span),
                        );
                        return;
                    }
                    TyIr::Entity
                    | TyIr::Query
                    | TyIr::Bool
                    | TyIr::Char
                    | TyIr::Int
                    | TyIr::Float
                    | TyIr::Str
                    | TyIr::ADT(_) => {
                        let Some((_, file)) = ty_ctxt.global.get_span(impl_id) else {
                            ty_ctxt.diag.push_error(
                                EcslError::new(ErrorLevel::Error, "Cannot impl for type")
                                    .with_span(|_| ty.span),
                            );
                            return;
                        };

                        if file != ty_ctxt.file {
                            ty_ctxt.diag.push_error(
                                EcslError::new(
                                    ErrorLevel::Error,
                                    &format!(
                                        "Impl block for Type '{}' must be in same file",
                                        impl_id
                                    ),
                                )
                                .with_span(|_| ty.span),
                            );
                            return;
                        }
                    }
                    _ => todo!(),
                }

                let parent = FieldDef {
                    id: FieldID::ZERO,
                    ty: ty_ctxt.get_tyid(ty, &scope).unwrap(),
                    params: ty
                        .generics
                        .params
                        .iter()
                        .map(|ty| ty_ctxt.get_tyid(ty, &scope).unwrap())
                        .collect(),
                };

                parse_function(fnid, Some(parent), f, scope);

                scope.pop();
            }
        }
    };

    let mut scope = GenericsScope::new();
    let defined = ty_ctxt.defined.read().unwrap();
    let assoc = ty_ctxt.assoc.read().unwrap();

    defined
        .iter()
        .for_each(|(_, def)| process_def(def, &mut scope));

    assoc.iter().for_each(|(_, ctxt)| {
        ctxt.iter()
            .for_each(|(_, def)| process_def(def, &mut scope))
    });
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
