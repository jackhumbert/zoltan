use clang::diagnostic::Severity;
use clang::{Clang, EntityKind, EntityVisitResult, Index};
use error::{Error, Result};
use flexi_logger::{LogSpecification, Logger};
use zoltan::opts::Opts;
use zoltan::spec::FunctionSpec;
use zoltan::types::Type;

use crate::resolver::TypeResolver;

mod error;
mod resolver;

fn main() {
    Logger::with(LogSpecification::info()).start().unwrap();

    let opts = Opts::load("Zoltan Clang frontend for C/C++");
    match run(&opts) {
        Ok(()) => log::info!("Finished!"),
        Err(err) => {
            log::error!("{err}");
            std::process::exit(1);
        }
    }
}

fn run(opts: &Opts) -> Result<()> {
    let clang = Clang::new().unwrap();
    let index = Index::new(&clang, true, false);

    log::info!("Parsing sources...");

    let unit = index
        .parser(&opts.source_path)
        .arguments(&opts.compiler_flags)
        .skip_function_bodies(true)
        .parse()?;

    let diagnostics = unit.get_diagnostics();
    if diagnostics
        .iter()
        .any(|err| err.get_severity() == Severity::Error)
    {
        return Err(Error::from_diagnostics(diagnostics));
    }

    log::info!("Searching for typedefs...");

    let mut resolver = TypeResolver::new(opts.strip_namespaces);
    let mut entities = vec![];
    let mut functions = vec![];
    let mut vars = vec![];

    unit.get_entity().visit_children(|ent, _| {
        let is_project_file = ent
            .get_location()
            .and_then(|loc| loc.get_file_location().file)
            .map(|file| file.get_path())
            .as_deref()
            == Some(&opts.source_path);

        match ent.get_kind() {
            EntityKind::Namespace => EntityVisitResult::Recurse,
            EntityKind::TypedefDecl | EntityKind::TypeAliasDecl if is_project_file || ent.get_comment().is_some() => {
                entities.push(ent);
                EntityVisitResult::Continue
            }
            EntityKind::VarDecl => {
                vars.push(ent);
                EntityVisitResult::Continue
            }
            EntityKind::Method 
            | EntityKind::FunctionDecl 
            | EntityKind::Constructor 
            | EntityKind::Destructor => {
                // if let Some(name) = ent.get_name() {
                //     if name == "GetVehDriveModelDataAI" {
                //         log::info!("{}", name);
                //     }
                // }
                functions.push(ent);
                EntityVisitResult::Continue
            },
            EntityKind::StructDecl
            | EntityKind::ClassDecl
            | EntityKind::UnionDecl
            | EntityKind::EnumDecl =>
            {
                if opts.eager_type_export {
                    resolver.resolve_decl(ent).ok();
                }
                EntityVisitResult::Recurse
            }
            _ => EntityVisitResult::Continue,
        }
    });

    let mut specs = vec![];
    for ent in entities {
        if let Some(comment) = ent.get_comment() {
            if let Type::Function(typ) = resolver.resolve_type(ent.get_type().unwrap())? {
                let name = ent.get_name_raw().unwrap().as_str().into();
                if let Some(spec) = FunctionSpec::new(name, name, typ, comment.as_str().lines()) {
                    specs.push(spec?);
                }
            }
        }
    }
    for ent in vars {
        if let Some(comment) = ent.get_comment() {
            if let Type::Long(_typ) = resolver.resolve_type(ent.get_type().unwrap())? {
                let mut name = ent.get_name_raw().unwrap().as_str().into();
                let fun_type = zoltan::types::FunctionType::new(vec![], resolver.resolve_type(ent.get_type().unwrap()).unwrap()).into();
                if let Some(parent) = ent.get_lexical_parent() && let Some(parent_name) = resolver.get_parent_name(parent) {
                    name = format!("{}_{}", parent_name, name).into();
                }
                if let Some(spec) = FunctionSpec::new(name, name, fun_type, comment.as_str().lines()) {
                    specs.push(spec?);
                }
            }
        }
    }
    for ent in functions {
        if let Some(comment) = ent.get_comment() {
            if let Type::Function(typ) = resolver.resolve_type(ent.get_type().unwrap())? {
                let mut name = ent.get_name_raw().unwrap().as_str().to_owned();
                let mut full_name = name.clone();
                let mut alt_typ = typ.to_owned();
                let mut params = vec![];
                if let Some(parent) = ent.get_lexical_parent() {
                    let is_constructor = ent.get_kind() == clang::EntityKind::Constructor;
                    let is_destructor = ent.get_kind() == clang::EntityKind::Destructor;
                    let is_user_code = parent.get_kind() == clang::EntityKind::TranslationUnit;
                    if let Some(parent_name) = resolver.get_parent_name(parent) {
                        if is_constructor {
                            full_name = parent_name;
                        } else if is_destructor {
                            full_name = format!("__{}", parent_name);
                        } else if !is_user_code {
                            name = format!("{}_{}", parent_name, name);
                        }
                    }
                    if let Some(parent_type) = parent.get_type() {
                        if let Some(parent_typ) = resolver.resolve_type(parent_type).ok() {
                            params.push(Type::Pointer(parent_typ.into()));
                            params = [params, typ.params.clone()].concat();
                            alt_typ = zoltan::types::FunctionType::new(params, typ.return_type.clone()).into();
                        }
                    }
                }
                if let Some(spec) = FunctionSpec::new(name.into(), full_name.into(), alt_typ, comment.lines()) {
                    specs.push(spec?);
                }
            }
        }
    }

    zoltan::process_specs(specs, &resolver.into_types(), opts)?;

    Ok(())
}