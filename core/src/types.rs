use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::hash::BuildHasherDefault;
use std::rc::Rc;

use auto_enums::auto_enum;
use derive_more::{AsRef, From};
use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use ustr::{IdentityHasher, Ustr};
use std::hash::{Hash, Hasher};

pub const POINTER_SIZE: usize = 8;
pub const MAX_ALIGN: usize = 8;

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum Type {
    Void,
    Bool,
    Char(bool),
    WChar,
    Short(bool),
    Int(bool),
    Long(bool),
    Float,
    Double,
    Pointer(Rc<Type>),
    Reference(Rc<Type>),
    Array(Rc<Type>),
    FixedArray(Rc<Type>, usize),
    Function(Rc<FunctionType>),
    // VirtualFunction(Rc<FunctionType>),
    // StaticFunction(Rc<FunctionType>),
    Union(UnionId),
    Struct(StructId),
    Enum(EnumId),
    Constant(Rc<Type>),
}

impl Type {
    pub fn size(&self, info: &TypeInfo) -> Option<usize> {
        match self {
            Type::Void => Some(0),
            Type::Bool => Some(1),
            Type::Char(_) => Some(1),
            #[cfg(windows)]
            Type::WChar => Some(2),
            #[cfg(unix)]
            Type::WChar => Some(4),
            Type::Short(_) => Some(2),
            Type::Int(_) => Some(4),
            Type::Long(_) => Some(8),
            Type::Float => Some(4),
            Type::Double => Some(8),
            Type::Pointer(_) => Some(POINTER_SIZE),
            Type::Reference(_) => Some(POINTER_SIZE),
            Type::Array(_) => None,
            Type::FixedArray(ty, size) => ty.size(info).map(|v| v * size),
            Type::Function(_) => Some(POINTER_SIZE),
            // Type::VirtualFunction(_) => Some(POINTER_SIZE),
            // Type::StaticFunction(_) => Some(POINTER_SIZE),
            Type::Union(u) => info.unions.get(u).and_then(|u| u.size),
            Type::Struct(s) => info.structs.get(s).and_then(|s| s.size),
            Type::Enum(e) => info.enums.get(e).and_then(|e| e.size),
            Type::Constant(ty) => ty.size(info),
        }
    }

    fn name_right(&self, parent: Option<Type>) -> Option<Cow<'static, str>> {
        match self {
            Type::Pointer(inner) => inner.name_right(Some(self.to_owned())),
            Type::Reference(inner) => inner.name_right(Some(self.to_owned())),
            Type::Array(inner) => Some(
                format!(
                    "{}[]",
                    inner.name_right(Some(self.to_owned())).unwrap_or_default()
                )
                .into(),
            ),
            Type::FixedArray(inner, size) => Some(
                format!(
                    "{}[{size}]",
                    inner.name_right(Some(self.to_owned())).unwrap_or_default()
                )
                .into(),
            ),
            Type::Function(ty) => {
                let params = ty.params.iter().map(Type::name).format(", ");
                match parent {
                    Some(Type::Pointer(_)) | Some(Type::Reference(_)) => {
                        Some(format!(")({params})").into())
                    }
                    _ => Some(format!("({params})").into()),
                }
            }
            // Type::VirtualFunction(ty) => {
            //     let params = ty.params.iter().map(Type::name).format(", ");
            //     Some(format!("({params})").into())
            // }
            // Type::StaticFunction(ty) => {
            //     let params = ty.params.iter().map(Type::name).format(", ");
            //     Some(format!("({params})").into())
            // }
            _ => None,
        }
    }

    fn name_left(&self) -> Cow<'static, str> {
        match self {
            Type::Void => "void".into(),
            Type::Bool => "bool".into(),
            Type::Char(true) => "int8_t".into(),
            Type::Char(false) => "uint8_t".into(),
            Type::WChar => "wchar_t".into(),
            Type::Short(true) => "int16_t".into(),
            Type::Short(false) => "uint16_t".into(),
            Type::Int(true) => "int32_t".into(),
            Type::Int(false) => "uint32_t".into(),
            Type::Long(true) => "int64_t".into(),
            Type::Long(false) => "uint64_t".into(),
            Type::Float => "float".into(),
            Type::Double => "double".into(),
            Type::Union(id) => id.to_string().into(),
            Type::Struct(id) => id.to_string().into(),
            Type::Enum(id) => id.to_string().into(),
            Type::Pointer(inner) if matches!(inner.as_ref(), Type::Function(_)) => {
                format!("{}(*", inner.name_left()).into()
            }
            Type::Pointer(inner) => format!("{}*", inner.name_left()).into(),
            Type::Reference(inner) if matches!(inner.as_ref(), Type::Function(_)) => {
                format!("{}(&", inner.name_left()).into()
            }
            Type::Reference(inner) => format!("{}&", inner.name_left()).into(),
            Type::Array(inner) => inner.name_left(),
            Type::FixedArray(inner, _) => inner.name_left(),
            Type::Function(fun) => fun.return_type.name(),
            Type::Constant(inner) => format!("const {}", inner.name_left()).into()
            // Type::VirtualFunction(fun) => fun.return_type.name(),
            // Type::StaticFunction(fun) => fun.return_type.name(),
        }
    }

    pub fn name(&self) -> Cow<'static, str> {
        match self.name_right(None) {
            Some(right) => format!("{}{right}", self.name_left()).into(),
            None => self.name_left(),
        }
    }

    pub fn name_with_id(&self, id: &str) -> Cow<'static, str> {
        match self.name_right(None) {
            Some(right) => format!("{} {id}{right}", self.name_left()).into(),
            None => format!("{} {id}", self.name_left()).into(),
        }
    }
}

pub fn format_name_for_idc(s: Ustr) -> Ustr {
    s.replace('<', "_")
        .replace('>', "")
        .replace(',', "_")
        .replace('*', "_p")
        .replace('&', "_r")
        .replace(' ', "")
        .replace('~', "__")
        .into()
}

trait IdcFormat: std::fmt::Display {
    fn to_idc_string(s: &str) -> String {
        format!(
            "{}",
            s.replace('<', "_")
                .replace('>', "")
                .replace(',', "_")
                .replace('*', "_p")
                .replace('&', "_r")
                .replace(' ', "")
                .replace('~', "__")
        )
    }
}

#[derive(Debug, Clone, Copy, AsRef, From, Ord, PartialOrd)]
pub struct StructId(Ustr);

impl fmt::Display for StructId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write!(f, "{}", format_name_for_idc(self.0))
        write!(f, "{}", self.0)
    }
}

impl PartialEq for StructId {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Hash for StructId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Eq for StructId {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AsRef, From, Hash, Ord, PartialOrd)]
pub struct UnionId(pub Ustr);

impl fmt::Display for UnionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write!(f, "{}", format_name_for_idc(self.0))
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AsRef, From, Hash, Ord, PartialOrd)]
pub struct EnumId(Ustr);

impl fmt::Display for EnumId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write!(f, "{}", format_name_for_idc(self.0))
        write!(f, "{}", self.0)
    }
}

pub type TypeMap<K, V> = HashMap<K, V, BuildHasherDefault<IdentityHasher>>;

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionEnum {
    Method,
    Virtual,
    Static,
    Typedef,
    Constructor,
    Destructor,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub func_type: FunctionEnum,
}

impl FunctionType {
    pub fn new(params: Vec<Type>, return_type: Type, func_type: FunctionEnum) -> Self {
        Self {
            params,
            return_type,
            func_type,
        }
    }
    pub fn new_vft(parent: Type, params: Vec<Type>, return_type: Type, func_type: FunctionEnum) -> Self {
        Self {
            params: vec![vec![parent], params].concat(),
            return_type,
            func_type,
        }
    }
}

#[derive(Debug)]
pub struct DataMember {
    pub name: Ustr,
    pub typ: Type,
    pub bit_offset: Option<usize>,
    pub is_bitfield: bool,
    pub alignment: Option<usize>,
}

impl DataMember {
    pub fn basic(name: Ustr, typ: Type) -> Self {
        Self {
            name,
            typ,
            bit_offset: None,
            is_bitfield: false,
            alignment: None
        }
    }
}

#[derive(Debug)]
pub struct StructType {
    pub name: Ustr,
    pub nice_name: Option<Ustr>,
    pub base: Vec<StructId>,
    pub members: Vec<DataMember>,
    pub rva: u64,
    pub virtual_methods: Vec<Method>,
    pub overridden_virtual_methods: Vec<Method>,
    pub size: Option<usize>,
    pub alignment: Option<usize>,
}

impl StructType {
    pub fn stub(name: Ustr) -> Self {
        Self {
            name,
            nice_name: None,
            base: vec![],
            members: vec![],
            rva: 0,
            virtual_methods: vec![],
            overridden_virtual_methods: vec![],
            size: None,
            alignment: None,
        }
    }

    pub fn has_direct_virtual_methods(&self) -> bool {
        !self.virtual_methods.is_empty()
    }

    pub fn has_indirect_virtual_methods(&self, types: &TypeInfo) -> bool {
        self.base
            .first()
            .and_then(|id| types.structs.get(&id))
            .iter()
            .any(|typ| typ.has_virtual_methods(types))
    }

    pub fn has_virtual_methods(&self, types: &TypeInfo) -> bool {
        self.has_direct_virtual_methods() || self.has_indirect_virtual_methods(types)
    }

    #[auto_enum(Iterator)]
    pub fn all_members<'a>(&'a self, types: &'a TypeInfo) -> impl Iterator<Item = &'a DataMember> {
        match self.base.first().and_then(|id| types.structs.get(&id)) {
            Some(typ) => {
                Box::new(typ.all_members(types).chain(self.members.iter())) as Box<dyn Iterator<Item = _>>
            }
            None => self.members.iter(),
        }
    }

    #[auto_enum(Iterator)]
    pub fn all_virtual_methods<'a>(&'a self, types: &'a TypeInfo) -> impl Iterator<Item = &'a Method> {
        match self.base.first().and_then(|id| types.structs.get(&id)) {
            Some(typ) => Box::new(typ.all_virtual_methods(types).chain(self.virtual_methods.iter()))
                as Box<dyn Iterator<Item = _>>,
            None => self.virtual_methods.iter(),
        }
    }
}

#[derive(Debug)]
pub struct Method {
    pub name: Ustr,
    pub full_name: Ustr,
    pub typ: Rc<FunctionType>,
    pub offset: u64,
}

#[derive(Debug)]
pub struct UnionType {
    pub name: Ustr,
    pub members: Vec<DataMember>,
    pub size: Option<usize>,
    pub alignment: Option<usize>,
}

#[derive(Debug)]
pub struct EnumType {
    pub name: Ustr,
    pub members: Vec<EnumMember>,
    pub size: Option<usize>,
    pub underlying_type: Option<Type>,
}

#[derive(Debug)]
pub struct EnumMember {
    pub name: Ustr,
    pub value: i64,
}

impl EnumMember {
    pub fn new(name: Ustr, value: i64) -> Self {
        Self { name, value }
    }
}

#[derive(Debug)]
pub struct TypeInfo {
    pub structs: TypeMap<StructId, StructType>,
    pub unions: TypeMap<UnionId, UnionType>,
    pub enums: TypeMap<EnumId, EnumType>,
}

#[derive(Debug, Default)]
pub struct NameAllocator {
    name_count: usize,
}

impl NameAllocator {
    pub fn allocate(&mut self) -> String {
        let i = self.name_count;
        self.name_count += 1;
        format!("__anonymous{}", i)
    }
}
