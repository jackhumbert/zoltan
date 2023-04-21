use std::collections::HashMap;
use std::rc::Rc;

use ustr::Ustr;

use crate::error::{Result, SymbolError};
use crate::eval::EvalContext;
use crate::exe::ExecutableData;
use crate::patterns;
use crate::spec::FunctionSpec;
use crate::types::{FunctionType, Type};

pub fn resolve_in_exe(
    specs: Vec<FunctionSpec>,
    exe: &ExecutableData,
) -> Result<(Vec<FunctionSymbol>, Vec<SymbolError>, Vec<FunctionSymbol>)> {
    let mut match_map: HashMap<usize, Vec<u64>> = HashMap::new();
    for mat in patterns::multi_search(specs.iter().map(|spec| &spec.pattern), exe.text()) {
        match_map.entry(mat.pattern).or_default().push(mat.rva + exe.text_offset_from_base());
    }
    // also look through RDATA - this will cause issues with duplicate finds (if any exist)
    for mat in patterns::multi_search(specs.iter().map(|spec| &spec.pattern), exe.rdata()) {
        match_map.entry(mat.pattern).or_default().push(mat.rva + exe.rdata_offset_from_base());
    }

    let mut syms = vec![];
    let mut notf = vec![];
    let mut errs = vec![];
    for (i, fun) in specs.into_iter().enumerate() {
        match match_map.get(&i).map(|vec| &vec[..]) {
            Some([addr]) => syms.push(resolve_symbol(fun, exe, *addr)?),
            Some(addrs) => {
                if let Some((n, max)) = fun.nth_entry_of {
                    match addrs.get(n) {
                        Some(rva) if max == addrs.len() || max == 0 => syms.push(resolve_symbol(fun, exe, *rva)?),
                        Some(_) => {
                            errs.push(SymbolError::CountMismatch(fun.full_name, addrs.len()));
                            notf.push(FunctionSymbol::new(fun.name, fun.full_name, fun.spec_type, 0, fun.file_name, fun.needs_impl));
                        },
                        None => {
                            errs.push(SymbolError::NotEnoughMatches(fun.full_name, addrs.len()));
                            notf.push(FunctionSymbol::new(fun.name, fun.full_name, fun.spec_type, 0, fun.file_name, fun.needs_impl));
                        },
                    }
                } else {
                    errs.push(SymbolError::MoreThanOneMatch(fun.full_name, addrs.len()));
                    notf.push(FunctionSymbol::new(fun.name, fun.full_name, fun.spec_type, 0, fun.file_name, fun.needs_impl));
                }
            }
            None => { 
                errs.push(SymbolError::NoMatches(fun.full_name));
                notf.push(FunctionSymbol::new(fun.name, fun.full_name, fun.spec_type, 0, fun.file_name, fun.needs_impl));
            },
        }
    }
    Ok((syms, errs, notf))
}

fn resolve_symbol(spec: FunctionSpec, data: &ExecutableData, rva: u64) -> Result<FunctionSymbol> {
    let res = match &spec.eval {
        Some(expr) => expr.eval(&EvalContext::new(&spec.pattern, data, data.rel_offset(rva))?)? - data.image_base(),
        None => (rva as i64 - spec.offset.unwrap_or(0) as i64) as u64,
    };
    Ok(FunctionSymbol::new(spec.name, spec.full_name, spec.spec_type, res, spec.file_name, spec.needs_impl))
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSymbol {
    name: Ustr,
    full_name: Ustr,
    function_type: Type,
    rva: u64,
    file_name: Option<Ustr>,
    needs_impl: bool
}

impl FunctionSymbol {
    fn new(name: Ustr, full_name: Ustr, function_type: Type, rva: u64, file_name: Option<Ustr>, needs_impl: bool) -> Self {
        Self {
            name,
            full_name, 
            function_type,
            rva,
            file_name,
            needs_impl
        }
    }

    fn eq(&self, other: &FunctionSymbol) -> bool {
        self.full_name == other.full_name
    }

    pub fn func_name(&self) -> &str {
        let v: Vec<&str> = self.full_name.split("::").collect();
        v[v.len()-1]
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn context(&self) -> String {
        let mut v: Vec<&str> = self.full_name.split("::").collect();
        v.remove(v.len()-1);
        for (i, s) in v.clone().into_iter().enumerate() {
            if s.contains("/") || s.contains("\\") {
                v.remove(i);
            }
        }
        v.join("::")
    }

    pub fn full_name(&self) -> &str {
        &self.full_name
    }

    // pub fn full_name(&self) -> String {
    //     let mut v: Vec<&str> = self.full_name.split("::").collect();
    //     for (i, s) in v.clone().into_iter().enumerate() {
    //         if s.contains("/") || s.contains("\\") {
    //             v.remove(i);
    //         }
    //     }
    //     v.join("::")
    // }

    pub fn file_name(&self) -> &Option<Ustr> {
        &self.file_name
    }

    pub fn function_type(&self) -> Type {
        self.function_type.clone()
    }

    pub fn rva(&self) -> u64 {
        self.rva
    }
    pub fn needs_impl(&self) -> bool {
        self.needs_impl
    }
}
