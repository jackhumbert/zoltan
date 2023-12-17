use std::collections::HashSet;
use std::path::PathBuf;


use clang::{Clang, EntityKind, EntityVisitResult, Index};
use error::{Result};
use flexi_logger::{LogSpecification, Logger};
use rayon::prelude::*;
use glob::glob;

use zoltan::opts::Opts;
use zoltan::spec::FunctionSpec;
use zoltan::types::{FunctionEnum, Type};
use zoltan::ustr::Ustr;


use crate::resolver::TypeResolver;

mod error;
mod red;
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

fn run<'a>(opts: &Opts) -> Result<()> {
    let clang = Clang::new().unwrap();
    log::info!("Using {}", clang::get_version());
    let index = Index::new(&clang, true, opts.show_clang_errors);

    log::info!("Parsing sources...");

    let mut specs = vec![];
    let mut resolver = TypeResolver::new(opts.strip_namespaces);

    let mut entities = HashSet::new();
    let mut functions = HashSet::new();
    let mut vars = HashSet::new();
    // let mut units = vec![];

    let mut arguments = [opts.compiler_flags.iter().map(|f| f.replace('"', "").split(' ').map(str::to_owned).collect::<Vec<_>>()).flatten().collect::<Vec<_>>().as_slice(), &[
        // "-include".to_owned(),
        // "inttypes.h".to_owned(),
        // "-include",
        // "stdafx.hpp",
        "-ferror-limit=1000".to_owned()
    ]].concat();

    let paths = opts
        .source_path
        .iter()
        .map(|path| {
            if path.exists() && path.is_dir() {
                let path_str = path.as_os_str().to_str().unwrap().to_string();
                arguments.push(["-I", &path_str].join(""));
                let hpp = glob(&[&path_str, "**/*.hpp"].join("")).unwrap().map(Result::unwrap).collect::<Vec<PathBuf>>();
                let cpp = glob(&[&path_str, "**/*.cpp"].join("")).unwrap().map(Result::unwrap).collect::<Vec<PathBuf>>();
                vec![hpp, cpp].concat()
            } else {
                vec![path.to_path_buf()]
            }
        })
        .flatten().collect::<Vec<PathBuf>>();

    let filtered_paths = paths.iter().filter(|p| {
            let mut exists = p.exists();
            if !exists {
                log::error!("Could not find file: {}", p.as_os_str().to_str().unwrap());
            } else {
                let file = std::fs::read_to_string(p).unwrap();
                exists = file.contains("@pattern");
                // if !exists {
                    // log::warn!("Skipping {} (no patterns)", p.as_os_str().to_str().unwrap());
                // }
            }

            exists
        }).collect::<Vec<&PathBuf>>();

    log::info!("Arguments: {}", arguments.join(" "));

    // index.parser("C:\\Users\\Jack\\Documents\\cyberpunk\\in_world_navigation\\deps\\cyberpunk_cmake\\include\\Registrar.hpp").arguments(&arguments).single_file_parse(true).keep_going(true).incomplete(true).parse().expect("Ooooops");

    let units = filtered_paths
        .iter()
        .map(|p| {
            log::info!("Parsing {}...", p.as_os_str().to_str().unwrap());
            index
                .parser(p)
                .arguments(&arguments)
                .single_file_parse(opts.r4e_output_path.is_none() && opts.idc_output_path.is_none())
                // .single_file_parse(true)
                .detailed_preprocessing_record(true)
                .briefs_in_completion_results(true)
                // .incomplete(true)
                .keep_going(true)
                // .ignore_non_errors_from_included_files(true)
                .skip_function_bodies(true)
                .to_owned().parse().expect("Parsing error")
        }).collect::<Vec<_>>();

    // let units = parsers
    //     .filter_map(|p| p.parse().expect("Parsing error").into())
    //     .collect::<Vec<_>>();

    log::info!("Searching for typedefs...");

    units.iter().for_each(|unit| {
        // let diagnostics = unit.get_diagnostics();
        // if diagnostics
        //     .iter()
        //     .any(|err| err.get_severity() == Severity::Fatal)
        // {
        //     log::error!("{}", Error::from_diagnostics(diagnostics));
        // }
        // if diagnostics
        //         .iter()
        //         .any(|err| err.get_severity() == Severity::Warning)
        // {
        //     log::warn!("{}", Error::warning_from_diagnostics(diagnostics));
        // }

        let entity = unit.get_entity();
        entity.visit_children(|ent, _| {
            let is_project_file = if let Some(path) = ent
                .get_location()
                .and_then(|loc| loc.get_file_location().file)
                .map(|file| file.get_path())
                .as_deref() 
                {
                    paths.contains(&path.to_path_buf())
                } else {
                    true
                };
            if is_project_file || opts.eager_type_export || opts.r4e_output_path.is_some() {
                match ent.get_kind() {
                    EntityKind::InclusionDirective => {
                        // log::info!("{}", ent.get_name().unwrap_or("No name".to_owned()));
                        EntityVisitResult::Recurse
                    },
                    EntityKind::MacroDefinition |
                    EntityKind::MacroExpansion => {
                        EntityVisitResult::Recurse
                    },
                    EntityKind::Namespace => EntityVisitResult::Recurse,
                    EntityKind::TypedefDecl | EntityKind::TypeAliasDecl
                        if is_project_file || ent.get_comment().is_some() =>
                    {                        
                        entities.insert(ent);
                        EntityVisitResult::Continue
                    }
                    EntityKind::VarDecl => {
                        vars.insert(ent);
                        EntityVisitResult::Continue
                    }
                    EntityKind::Method
                    | EntityKind::FunctionDecl
                    | EntityKind::Constructor
                    | EntityKind::Destructor => {
                        // log::info!("Function: {}", ent.get_name().unwrap_or("unnamed".to_owned()));
                        functions.insert(ent);
                        EntityVisitResult::Continue
                    }
                    EntityKind::StructDecl
                    | EntityKind::ClassDecl
                    | EntityKind::UnionDecl
                    | EntityKind::EnumDecl => {
                        // if opts.eager_type_export {
                            // resolver.resolve_decl(ent).ok();
                        // }
                        EntityVisitResult::Recurse
                    }
                    _ => {
                        EntityVisitResult::Continue
                    },
                }
            } else {
                EntityVisitResult::Continue
            }
        });
    });

    log::info!("Found {} entities", entities.len());
    log::info!("Found {} functions", functions.len());
    log::info!("Found {} vars", vars.len());

    log::info!("Collecting specs...");

    for ent in entities {
        if let Some(comment) = ent.get_comment() {
            match resolver.resolve_type(ent.get_type().unwrap()) {
                Ok(Type::Function(typ)) => {
                    let name = ent.get_name_raw().unwrap().as_str().into();
                    let file_name: Option<Ustr> = if let Some(location) = ent.get_location() {
                        if let Some(file) = location.get_file_location().file {
                            Some(file.get_path().to_str().unwrap().to_string().into())
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    match FunctionSpec::new(
                        name,
                        name,
                        Type::Function(typ),
                        comment.as_str().lines(),
                        file_name,
                    ) {
                        Some(Ok(spec)) => specs.push(spec),
                        // Some(Err(err)) => log::warn!("{}", err),
                        _ => {}
                    }
                }
                Err(err) => {
                    log::warn!("{}", err);
                }
                _ => {}
            }
        }
    }
    for ent in vars {
        if let Some(comment) = ent.get_comment() {
            match resolver.resolve_type(ent.get_type().unwrap()).ok() {
                Some(Type::Constant(_)) | Some(Type::Long(_)) | Some(Type::Pointer(_)) => {
                    let name: Ustr = ent.get_name_raw().unwrap().as_str().into();
                    let mut full_name = name.clone();
                    let var_type = resolver.resolve_type(ent.get_type().unwrap()).unwrap();
                    if let Some(parent) = ent.get_lexical_parent() {
                        if let Some(parent_name) = resolver.get_parent_name(parent) {
                            full_name = format!("{}::{}", parent_name, name).into();
                        }
                    }
                    let file_name: Option<Ustr> = if let Some(location) = ent.get_location() {
                        if let Some(file) = location.get_file_location().file {
                            Some(file.get_path().to_str().unwrap().to_string().into())
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    match FunctionSpec::new(name, full_name, var_type, comment.as_str().lines(), file_name)
                    {
                        Some(Ok(spec)) => specs.push(spec),
                        // Some(Err(err)) =>  log::warn!("{}", err),
                        _ => {}
                    }
                }
                _ => {}
            }
        }
    }
    for ent in functions {
        if let Some(comment) = ent.get_comment() {
            // println!("{}", comment.as_str());
            if let Some(Type::Function(typ)) = resolver.resolve_type(ent.get_type().unwrap()).ok() {
                let name = ent.get_name_raw().unwrap().as_str().to_owned();
                let mut full_name = name.clone();
                let mut alt_typ = typ.to_owned();
                let mut params = vec![];
                // let mut spec_type: Type;
                if let Some(parent) = ent.get_lexical_parent() {
                    let is_constructor = ent.get_kind() == clang::EntityKind::Constructor;
                    let is_destructor = ent.get_kind() == clang::EntityKind::Destructor;
                    // let is_user_code = parent.get_kind() == clang::EntityKind::TranslationUnit || ent.get_kind() == clang::EntityKind::TranslationUnit;
                    if let Some(parent_name) = resolver.get_parent_name(parent) {
                        full_name = format!("{}::{}", parent_name, name);
                    }
                    if let Some(parent_type) = parent.get_type() {
                        if let Some(parent_typ) = resolver.resolve_type(parent_type).ok() {
                            params.push(Type::Pointer(parent_typ.into()));
                            params = [params, typ.params.clone()].concat();
                            let func_type: FunctionEnum;
                            if ent.is_virtual_method() {
                                func_type = FunctionEnum::Virtual;
                            } else if ent.is_static_method() {
                                func_type = FunctionEnum::Static;
                            } else if is_constructor {
                                func_type = FunctionEnum::Constructor;
                            } else if is_destructor {
                                func_type = FunctionEnum::Destructor;
                            } else {
                                func_type = FunctionEnum::Method;
                            }
                            alt_typ = zoltan::types::FunctionType::new(
                                params,
                                typ.return_type.clone(),
                                func_type,
                            )
                            .into();
                        }
                    }
                }

                // let mut cur_ent = ent;
                let file_name: Option<Ustr> = if let Some(location) = ent.get_location() {
                    if let Some(file) = location.get_file_location().file {
                        Some(file.get_path().to_str().unwrap().to_string().into())
                    } else {
                        None
                    }
                } else {
                    None
                };
                // let mut file_name: Option<Ustr> = Some(ent.get_file().unwrap().get_path().into_os_string().into_string().unwrap().into());
                // while let Some(parent) = cur_ent.get_lexical_parent() {
                //     if parent.get_kind() == clang::EntityKind::TranslationUnit {
                //         file_name = Some(parent.get_display_name().unwrap().into());
                //         break
                //     } else {
                //         cur_ent = parent;
                //     }
                // }
                // let file_name = ent.get_lexical_parent();
                match FunctionSpec::new(
                    name.into(),
                    full_name.into(),
                    Type::Function(alt_typ),
                    comment.lines(),
                    file_name,
                ) {
                    Some(Ok(spec)) => specs.push(spec),
                    // Some(Err(err)) =>  log::warn!("{}", err),
                    _ => {}
                }
            }
        }
    }

    log::info!("Found {} specs", specs.len());

    zoltan::process_specs(specs, &resolver.into_types(), opts)?;

    Ok(())
}
