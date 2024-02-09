use std::hash::BuildHasherDefault;

// use std::path::Path;
use quickscope::ScopeMap;
use zoltan::types::*;
use zoltan::ustr::{IdentityHasher, Ustr};

use crate::error::{Error, Result};

pub struct TypeResolver {
    structs: TypeMap<StructId, StructType>,
    unions: TypeMap<UnionId, UnionType>,
    enums: TypeMap<EnumId, EnumType>,
    local_types: ScopeMap<Ustr, Type, BuildHasherDefault<IdentityHasher>>,
    name_allocator: NameAllocator,
    strip_namespaces: bool,
}

impl TypeResolver {
    pub fn new(strip_namespaces: bool) -> Self {
        Self {
            structs: TypeMap::default(),
            unions: TypeMap::default(),
            enums: TypeMap::default(),
            local_types: ScopeMap::default(),
            name_allocator: NameAllocator::default(),
            strip_namespaces,
        }
    }

    pub fn into_types(self) -> TypeInfo {
        TypeInfo {
            structs: self.structs,
            unions: self.unions,
            enums: self.enums,
        }
    }

    pub fn resolve_decl(&mut self, entity: clang::Entity) -> Result<Type> {
        // let name: Ustr = entity.get_name().unwrap().into();
        // let context = self.generate_context_name(entity);
        let name = self.generate_context_name(entity);

        match entity.get_kind() {
            clang::EntityKind::StructDecl
            | clang::EntityKind::ClassDecl
            | clang::EntityKind::ClassTemplate => {
                // let needs_resolved = match self.structs.get(&name.into()) {
                //     Some(_) => !entity.get_children().is_empty(),
                //     None => true,
                // };
                if !self.structs.contains_key(&name.into()) {
                    // if !entity.get_children().is_empty() {
                    let size = entity.get_type().and_then(|t| t.get_sizeof().ok());
                    let alignment = entity.get_type().and_then(|t| t.get_alignof().ok());
                    let mut ent = None;
                    if let Some(template) = entity.get_template() {
                        self.structs.insert(name.into(), StructType::stub(name));
                        let res = self.resolve_struct(name, template, size, alignment);
                        ent = res.ok();
                    } else {
                        if !entity.get_children().is_empty() {
                            self.structs.insert(name.into(), StructType::stub(name));
                            ent = self.resolve_struct(name, entity, size, alignment).ok();
                        }
                    }
                    if ent.is_some() {
                        self.structs.insert(name.into(), ent.unwrap());
                    }
                    // }
                }
                Ok(Type::Struct(name.into()))
            }
            clang::EntityKind::EnumDecl => {
                if !self.enums.contains_key(&name.into()) {
                    let res = self.resolve_enum(name, entity)?;
                    self.enums.insert(name.into(), res);
                }
                Ok(Type::Enum(name.into()))
            }
            clang::EntityKind::UnionDecl => {
                if !self.unions.contains_key(&name.into()) {
                    let res = self.resolve_union(name, entity)?;
                    self.unions.insert(name.into(), res);
                }

                Ok(Type::Union(name.into()))
            }
            other => Err(Error::UnexpectedKind(other)),
        }
    }

    pub fn get_template_type(&mut self, entity: clang::Entity) -> Option<Type> {
        let mut p_type: Option<Type> = None;
        if let Some(e_typ) = entity.get_type() {
            if let Some(args) = e_typ.get_template_argument_types() {
                // log::info!("get_template_arguments");
                // match args.first() {
                //     Some(clang::TemplateArgument::Type(typ)) => {
                //         log::info!("first && Type");
                //         p_type = self.resolve_type(*typ).ok();
                //     },
                //     Some(_) => {
                //         log::info!("first && other");
                //     },
                //     None => {}
                // }
                // self.local_types.push_layer();

                let decl = e_typ.get_declaration().unwrap();
                let template = if decl.get_kind() == clang::EntityKind::ClassTemplate {
                    decl
                } else {
                    decl.get_template().unwrap()
                };

                for (ent, typ) in template
                    .get_children()
                    .iter()
                    .take_while(|ent| ent.get_kind() == clang::EntityKind::TemplateTypeParameter)
                    .zip(&args)
                {
                    if let Some(typ) = typ {
                        let typ = self.resolve_type(*typ).unwrap();
                        self.local_types
                            .define(ent.get_display_name().unwrap().as_str().into(), typ.clone());
                        p_type = Some(typ);
                        break;
                    }
                }
            }
        }
        return p_type;
    }

    pub fn resolve_type(&mut self, typ: clang::Type) -> Result<Type> {
        // populate template arguments if available
        if let Some(args) = typ.get_template_argument_types() {
            self.local_types.push_layer();

            if let Some(decl) = typ.get_declaration() {
                let template = if decl.get_kind() == clang::EntityKind::ClassTemplate {
                    Some(decl)
                // } else if decl.get_kind() == clang::EntityKind::FunctionTemplate {
                // Some(decl)
                } else {
                    decl.get_template()
                };

                if let Some(temp) = template {
                    for (ent, typ) in temp
                        .get_children()
                        .iter()
                        .take_while(|ent| ent.get_kind() == clang::EntityKind::TemplateTypeParameter)
                        // .take_while(|ent| ent.get_kind() == clang::EntityKind::TemplateTemplateParameter)
                        .zip(&args)
                    {
                        if let Some(typ) = typ {
                            let typ = self.resolve_type(*typ)?;
                            self.local_types
                                .define(ent.get_name_raw().unwrap().as_str().into(), typ);
                        }
                    }
                }
            }
        }
        // let name = typ.get_display_name();
        // log::info!("{}", &name);

        let mut res = match typ.get_kind() {
            clang::TypeKind::Void => Type::Void,
            clang::TypeKind::Bool => Type::Bool,
            clang::TypeKind::CharS | clang::TypeKind::SChar => Type::Char(true),
            clang::TypeKind::CharU | clang::TypeKind::UChar => Type::Char(false),
            clang::TypeKind::WChar => Type::WChar,
            clang::TypeKind::Short => Type::Short(true),
            clang::TypeKind::UShort => Type::Short(false),
            clang::TypeKind::Int => Type::Int(true),
            clang::TypeKind::UInt => Type::Int(false),
            clang::TypeKind::Long | clang::TypeKind::LongLong => Type::Long(true),
            clang::TypeKind::ULong | clang::TypeKind::ULongLong => Type::Long(false),
            clang::TypeKind::Float => Type::Float,
            clang::TypeKind::Double => Type::Double,
            clang::TypeKind::Pointer => {
                let inner = self.resolve_type(typ.get_pointee_type().unwrap())?;
                Type::Pointer(inner.into())
            }
            clang::TypeKind::LValueReference | clang::TypeKind::RValueReference => {
                let inner = self.resolve_type(typ.get_pointee_type().unwrap())?;
                Type::Reference(inner.into())
            }
            clang::TypeKind::Enum => self.resolve_decl(typ.get_declaration().unwrap())?,
            clang::TypeKind::Record => self.resolve_decl(typ.get_declaration().unwrap())?,
            clang::TypeKind::Typedef => self.resolve_type(typ.get_canonical_type())?,
            clang::TypeKind::FunctionPrototype => {
                let fun = self.resolve_function(typ)?;
                Type::Function(fun.into())
            }
            clang::TypeKind::FunctionNoPrototype => todo!(),
            clang::TypeKind::ConstantArray => {
                let inner = self.resolve_type(typ.get_element_type().unwrap())?;
                Type::FixedArray(inner.into(), typ.get_size().unwrap())
            }
            clang::TypeKind::DependentSizedArray => {
                let inner = self.resolve_type(typ.get_element_type().unwrap())?;
                let size = typ.get_size().unwrap_or(0);
                Type::FixedArray(inner.into(), size)
            }
            clang::TypeKind::Elaborated => self.resolve_type(typ.get_elaborated_type().unwrap())?,
            clang::TypeKind::Unexposed => {
                if typ.get_template_argument_types().is_some() {
                    // type with template arguments
                    self.resolve_decl(typ.get_declaration().unwrap())?
                } else {
                    // template argument
                    let name = typ.get_display_name().into();
                    // log::info!("{}", &name);
                    self.local_types
                        .get(&name)
                        .ok_or(Error::UnresolvedType(name))?
                        .clone()
                }
            }
            other => return Err(Error::UnexpectedType(other)),
        };

        // not sure if this handles const functions correctly with `this` in param list
        if typ.is_const_qualified() {
            res = Type::Constant(res.into());
        }

        if typ.get_template_argument_types().is_some() {
            self.local_types.pop_layer();
        }
        Ok(res)
    }

    fn resolve_struct(
        &mut self,
        name: Ustr,
        entity: clang::Entity,
        size: Option<usize>,
        alignment: Option<usize>,
    ) -> Result<StructType> {
        let children = entity.get_children();
        // if children.len() == 0 {
        //     Error::CompilerErrors("no children".to_owned());
        // }
        let mut base = vec![];
        // let base = children
        //     .iter()
        //     .find(|ent| ent.get_kind() == clang::EntityKind::BaseSpecifier)
        //     .and_then(|ent| ent.get_definition())
        //     .map(|ent| self.resolve_decl(ent))
        //     .transpose()?
        //     .and_then(|ty| ty.into_struct().ok());

        let mut members = vec![];
        let mut virtual_methods = vec![];
        let mut overridden_virtual_methods = vec![];
        let mut rva = 0;
        // if let Some(cmt) = entity.get_comment_brief() {
        //     log::info!("{name}: {}", cmt);
        //     if let Ok(v) = cmt.trim().trim_start_matches("RVA=").parse() {
        //         rva = v;
        //     }
        // }
        let mut base_entity = None;
        let mut vft_base = 0;
        let mut vft_index = 0;
        let nice_name: Option<Ustr> = None;

        // if name == "ISerializable" {
        //     log::info!("ISerializable:")
        // }
        for child in children {
            // if name == "ISerializable" {
            //     log::info!("* {:?}", child.get_kind());
            // }
            match child.get_kind() {
                clang::EntityKind::BaseSpecifier => {
                    if let Some(base_o) = child
                        .get_definition()
                        .map(|ent| self.resolve_decl(ent))
                        .transpose()?
                        .and_then(|ty| ty.into_struct().ok())
                    {
                        base.push(base_o);
                    }
                    if vft_base == 0 {
                        vft_base = self.get_vft_base(child);
                        base_entity = Some(child);
                    }
                }
                clang::EntityKind::VarDecl => {
                    let var_name = self.get_entity_name(child);
                    if var_name == "VFT" {
                        match child.evaluate() {
                            Some(clang::EvaluationResult::UnsignedInteger(u)) => {
                                rva = u;
                            }
                            _ => {}
                        }
                    } /*else if var_name == "NAME" {
                          match child.evaluate() {
                              Some(clang::EvaluationResult::String(s)) => {
                                  nice_name = Some(s.to_str().unwrap().into());
                              },
                              _ => {},
                          }
                      }*/
                }
                clang::EntityKind::FieldDecl => {
                    let name = self.get_entity_name(child);
                    let ctyp = child.get_type().unwrap();
                    let typ = self.resolve_type(ctyp)?;
                    let bit_offset = child.get_offset_of_field().ok();
                    let alignment = ctyp.get_alignof().ok();
                    members.push(DataMember {
                        name,
                        typ,
                        bit_offset,
                        is_bitfield: child.is_bit_field(),
                        alignment,
                    })
                }
                clang::EntityKind::Method
                | clang::EntityKind::Destructor
                | clang::EntityKind::Constructor
                    if child.is_virtual_method() =>
                {
                    let is_override = child
                        .get_children()
                        .iter()
                        .any(|c| c.get_kind() == clang::EntityKind::OverrideAttr);
                    let is_final = child
                        .get_children()
                        .iter()
                        .any(|c| c.get_kind() == clang::EntityKind::FinalAttr);
                    let func_name = self.get_entity_name(child);
                    let mut full_name = func_name.clone();
                    // let mut full_name_clean = func_name.clone();
                    let is_constructor = child.get_kind() == clang::EntityKind::Constructor;
                    let is_destructor = child.get_kind() == clang::EntityKind::Destructor;
                    // let is_parent_file = entity.get_kind() == clang::EntityKind::TranslationUnit;
                    if let Some(parent_name) = self.get_parent_name(entity) {
                        if is_constructor {
                            full_name = parent_name.into();
                        } else if is_destructor {
                            full_name = format!("~{}", parent_name).into();
                            // full_name_clean = format!("__{}", parent_name.replace("::", "")).into();
                        } else {
                            full_name = format!("{}::{}", parent_name, func_name).into();
                            // full_name_clean = format!("{}_{}", parent_name.replace("::", ""), func_name).into();
                        }
                        // full_name_clean = full_name.replace("RED4ext", "").replace("::", "_").into();
                    }
                    if is_override || is_final {
                        if let Some(be) = base_entity {
                            let (found, offset) = self.get_func_vft_offset(be, func_name);
                            if found {
                                if let Type::Function(typ) = self.resolve_type(child.get_type().unwrap())? {
                                    overridden_virtual_methods.push(Method {
                                        name: func_name,
                                        full_name,
                                        typ: typ.clone(),
                                        offset,
                                    });
                                }
                            }
                        }
                    } else {
                        let offset = (vft_base + vft_index) as u64;
                        vft_index += 8;
                        if let Type::Function(typ) = self.resolve_type(child.get_type().unwrap())? {
                            virtual_methods.push(Method {
                                name: func_name,
                                full_name,
                                typ: typ.clone(),
                                offset,
                            });
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(StructType {
            name,
            nice_name,
            base,
            members,
            rva,
            virtual_methods,
            overridden_virtual_methods,
            size,
            alignment
        })
    }

    // pub fn get_red_name(&mut self, entity: clang::Entity) -> Option<String> {
    //     let mut red_name: Option<String> = None;
    //     for child in entity.get_children() {
    //         match child.get_kind() {
    //             clang::EntityKind::VarDecl => {
    //                 let var_name = self.get_entity_name(child);
    //                 if var_name == "NAME" {
    //                     match child.evaluate() {
    //                         Some(clang::EvaluationResult::String(s)) => {
    //                             red_name = Some(s.to_str().unwrap().into());
    //                         }
    //                         _ => {}
    //                     }
    //                 }
    //             }
    //             _ => {}
    //         }
    //     }
    //     red_name
    // }

    fn get_vft_base(&mut self, entity: clang::Entity) -> u64 {
        if let Some(def) = entity.get_definition() {
            let mut vft_base = 0;
            for child in def.get_children() {
                match child.get_kind() {
                    clang::EntityKind::BaseSpecifier => {
                        if vft_base == 0 {
                            vft_base += self.get_vft_base(child);
                        }
                    }
                    clang::EntityKind::Method
                    | clang::EntityKind::Destructor
                    | clang::EntityKind::Constructor
                        if child.is_virtual_method() =>
                    {
                        let is_override = child
                            .get_children()
                            .iter()
                            .any(|c| c.get_kind() == clang::EntityKind::OverrideAttr);
                        let is_final = child
                            .get_children()
                            .iter()
                            .any(|c| c.get_kind() == clang::EntityKind::FinalAttr);
                        if !is_override && !is_final {
                            vft_base += 8;
                        }
                    }
                    _ => {}
                }
            }
            vft_base as u64
        } else {
            0
        }
    }

    fn get_func_vft_offset(&mut self, entity: clang::Entity, name: Ustr) -> (bool, u64) {
        if let Some(def) = entity.get_definition() {
            let mut found = false;
            let mut vft_offset = 0;
            for child in def.get_children() {
                match child.get_kind() {
                    clang::EntityKind::BaseSpecifier => {
                        if vft_offset == 0 {
                            let (base_found, offset) = self.get_func_vft_offset(child, name);
                            if base_found {
                                found = true;
                                vft_offset = offset;
                                break;
                            } else {
                                vft_offset += offset
                            }
                        }
                    }
                    clang::EntityKind::Method
                    | clang::EntityKind::Destructor
                    | clang::EntityKind::Constructor
                        if child.is_virtual_method() =>
                    {
                        let is_override = child
                            .get_children()
                            .iter()
                            .any(|c| c.get_kind() == clang::EntityKind::OverrideAttr);
                        let is_final = child
                            .get_children()
                            .iter()
                            .any(|c| c.get_kind() == clang::EntityKind::FinalAttr);
                        if !is_override && !is_final {
                            let child_name = self.get_entity_name(child);
                            if child_name == name {
                                found = true;
                                break;
                            } else {
                                vft_offset += 8;
                            }
                        }
                    }
                    _ => {}
                }
            }
            (found, vft_offset as u64)
        } else {
            (false, 0)
        }
    }

    fn resolve_enum(&mut self, name: Ustr, entity: clang::Entity) -> Result<EnumType> {
        let children = entity.get_children();
        let mut members = vec![];

        for child in children {
            if child.get_kind() == clang::EntityKind::EnumConstantDecl {
                let name = self.get_entity_name(child);
                let (value, _) = child.get_enum_constant_value().unwrap();
                members.push(EnumMember { name, value });
            }
        }

        let underlying_type = entity
            .get_enum_underlying_type()
            .and_then(|t| self.resolve_type(t).ok());
        let size = entity.get_type().unwrap().get_sizeof().ok();
        Ok(EnumType {
            name,
            members,
            size,
            underlying_type,
        })
    }

    fn resolve_union(&mut self, name: Ustr, entity: clang::Entity) -> Result<UnionType> {
        let children = entity.get_children();
        let mut members = vec![];

        for child in children {
            if child.get_kind() == clang::EntityKind::FieldDecl {
                let name = self.get_entity_name(child);
                let ctyp = child.get_type().unwrap();
                let typ = self.resolve_type(ctyp)?;
                let bit_offset = child.get_offset_of_field().ok();
                let alignment = ctyp.get_alignof().ok();
                members.push(DataMember {
                    name,
                    typ,
                    bit_offset,
                    is_bitfield: false,
                    alignment,
                })
            }
        }

        let size = entity.get_type().unwrap().get_sizeof().ok();
        let alignment = entity.get_type().unwrap().get_sizeof().ok();
        Ok(UnionType { name, members, size, alignment })
    }

    fn resolve_function(&mut self, typ: clang::Type) -> Result<FunctionType> {
        let return_type = self.resolve_type(typ.get_result_type().unwrap())?;
        let mut params = vec![];
        let func_type = FunctionEnum::Typedef;

        for typ in typ.get_argument_types().unwrap() {
            params.push(self.resolve_type(typ)?);
        }

        Ok(FunctionType {
            return_type,
            params,
            func_type,
        })
    }

    // pub fn generate_type_name(&mut self, entity: clang::Entity) -> Ustr {
    //     let mut cur = Some(entity);
    //     let mut full_name = entity
    //         .get_display_name()
    //         .unwrap_or_else(|| self.name_allocator.allocate());

    //     // could use filename, but complicates some things
    //     // full_name = match entity.get_kind() {
    //     // clang::EntityKind::TranslationUnit => "".into(),
    //     // _ =>  full_name
    //     // };

    //     while let Some(parent) = cur {
    //         match parent.get_kind() {
    //             // clang::EntityKind::TranslationUnit => {}
    //             clang::EntityKind::Namespace if self.strip_namespaces => {}
    //             _ => {
    //                 let parent_name = parent.get_display_name();
    //                 let prefix = parent_name.as_deref().unwrap_or("__unnamed");
    //                 full_name = format!("{}::{}", prefix, full_name);
    //             }
    //         }
    //         cur = parent.get_lexical_parent();
    //     }

    //     full_name
    //         // .replace('<', "_")
    //         // .replace('>', "")
    //         // .replace(',', "_")
    //         // .replace('*', "_p")
    //         // .replace('&', "_r")
    //         // .replace(' ', "")
    //         .into()
    // }

    pub fn generate_context_name(&mut self, entity: clang::Entity) -> Ustr {
        let mut cur = Some(entity);
        let mut names: Vec<String> = vec![];

        // could use filename, but complicates some things
        // full_name = match entity.get_kind() {
        //     clang::EntityKind::TranslationUnit => Path::new(& full_name).file_stem().unwrap().to_os_string().into_string().unwrap(),
        //     _ =>  full_name
        // };

        while let Some(parent) = cur {
            match parent.get_kind() {
                // clang::EntityKind::TranslationUnit => {}
                clang::EntityKind::Namespace if self.strip_namespaces => {}
                _ => {
                    let parent_name = parent
                        .get_display_name()
                        .unwrap_or(self.get_entity_name(parent).to_string());
                    if !parent_name.contains("/") && !parent_name.contains("\\") {
                        names.insert(0, parent_name.into());
                    }
                }
            }
            cur = parent.get_lexical_parent();
        }

        names
            .join("::")
            // .replace('<', "_")
            // .replace('>', "")
            // .replace(',', "_")
            // .replace('*', "_p")
            // .replace('&', "_r")
            // .replace(' ', "")
            .into()
    }

    fn get_entity_name(&mut self, entity: clang::Entity) -> Ustr {
        entity
            .get_name_raw()
            .map(|str| str.as_str().into())
            .unwrap_or_else(|| self.name_allocator.allocate().into())
    }

    pub fn get_parent_name(&mut self, ent: clang::Entity) -> Option<String> {
        // let parent_name = self.get_red_name(ent).or(Some(self.generate_type_name(ent).to_string())).unwrap(); //.replace("RED4ext", "").replace("::", "");
        let parent_name = self.generate_context_name(ent).to_string();
        if parent_name.eq("") {
            None
        } else {
            Some(parent_name)
        }
    }
}
