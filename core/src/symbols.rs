use std::collections::HashMap;
// use std::rc::Rc;

use ustr::Ustr;
use serde_json::Value;
use rayon::prelude::*;

use crate::error::{Result, SymbolError};
use crate::eval::EvalContext;
use crate::exe::ExecutableData;
use crate::patterns;
use crate::spec::{SymbolSpec, SegmentType};
use crate::types::Type;
use crate::codegen::format_name_for_addr;


pub fn resolve_in_exe(
    specs: Vec<SymbolSpec>,
    exe: &ExecutableData,
) -> Result<(Vec<FunctionSymbol>, Vec<SymbolError>, Vec<FunctionSymbol>)> {
    let mut strings: Vec<String> = vec![];

    for spec in specs.iter() {
        for pat in spec.pattern.parts() {
            match pat {
                patterns::PatItem::ByteCode(patterns::ByteCode::Lea(patterns::LeaType::Str(string))) => {
                    strings.push(string.to_owned());
                },
                _ => {}
            }
        }
    }

    let mut refs: HashMap<String, Vec<u64>> = HashMap::new();
    
    let mut syms = vec![];
    let mut notf = vec![];
    let mut errs = vec![];

    // load addresses from .json
    let file = std::fs::read_to_string("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\bin\\x64\\cyberpunk2077_addresses.json").unwrap();
    let list: Value = serde_json::from_str(&file).unwrap();
    for list2 in list.as_array().unwrap() {
        for value in list2.as_array().unwrap() {
            if value["symbol"].is_string() {
                refs.insert(format_name_for_addr(value["symbol"].as_str().unwrap().into()), vec![u64::from_str_radix(value["offset"].as_str().unwrap(), 16).unwrap() + 0x1000]);
                // syms.push(FunctionSymbol::new(
                //     value["symbol"].as_str().unwrap().into(), 
                //     value["symbol"].as_str().unwrap().into(), 
                //     Type::Void, 
                //     u64::from_str_radix(value["offset"].as_str().unwrap(), 16).unwrap() + 0x1000, 
                //     None, 
                //     false
                // ));       
            }
            if value["hash"].is_string() {                 
                refs.insert(value["hash"].as_str().unwrap().into(), vec![u64::from_str_radix(value["offset"].as_str().unwrap(), 16).unwrap() + 0x1000]);
                // syms.push(FunctionSymbol::new(
                //     format!("HASH_{}", value["hash"].as_str().unwrap()).into(), 
                //     format!("HASH_{}", value["hash"].as_str().unwrap()).into(), 
                //     Type::Void, 
                //     u64::from_str_radix(value["offset"].as_str().unwrap(), 16).unwrap() + 0x1000, 
                //     None, 
                //     false
                // ));
            }
        }
    }
    // load classes & vfts from .json (from vft ripper)
    let file = std::fs::read_to_string("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\bin\\x64\\cyberpunk2077_classes.json").unwrap();
    let list: Value = serde_json::from_str(&file).unwrap();
    for value in list.as_array().unwrap() {
        refs.insert(format_name_for_addr(value["symbol"].as_str().unwrap().into()), vec![u64::from_str_radix(value["offset"].as_str().unwrap(), 16).unwrap()]);
        syms.push(FunctionSymbol::new(
            value["symbol"].as_str().unwrap().into(), 
            value["symbol"].as_str().unwrap().into(), 
            Type::Void, 
            u64::from_str_radix(value["offset"].as_str().unwrap(), 16).unwrap(), 
            None, 
            false
        ));
    }
    // find strings used in byteCodes
    for string in strings {
        let mut v: Vec<u8> = string.as_bytes().into();
        v.push(0);
        let addrs: Vec<u64> = patterns::single_search(vec![v], exe.rdata()).iter().map(|x| x + exe.rdata_offset_from_base()).collect();
        refs.insert(string.to_owned(), addrs.clone());
        for addr in addrs {
            log::info!("{}: 0x{:X}", string, addr);
        }
    }
    // load common nullsubs (includes the end of some functions because of the short CC)
    refs.insert("null".to_owned(), patterns::single_search(vec![
            vec![0xC2, 0x00, 0x00, 0xCC]
        ], exe.text()).iter().map(|x| x + exe.text_offset_from_base()).collect());

    // load functions that return 0
    refs.insert("ret0".to_owned(), patterns::single_search(vec![
            vec![0x32, 0xC0, 0xC3, 0xCC],
            vec![0x33, 0xC0, 0xC3, 0xCC],
            vec![0xC2, 0x00, 0x00, 0xCC],
        ], exe.text()).iter().map(|x| x + exe.text_offset_from_base()).collect());

    let mut text_specs = vec![];
    let mut idata_specs = vec![];
    let mut rdata_specs = vec![];
    let mut data_specs = vec![];
    let mut pdata_specs = vec![];

    for spec in specs {
        match spec.segment {
            SegmentType::Any => {
                text_specs.push(spec);
            },
            SegmentType::Text => {
                text_specs.push(spec);
            },
            SegmentType::IData => {
                idata_specs.push(spec);
            }
            SegmentType::RData => {
                rdata_specs.push(spec);
            }
            SegmentType::Data => {
                data_specs.push(spec);
            }
            SegmentType::PData => {
                pdata_specs.push(spec);
            }
        }
    }    

    let mut all_specs = vec![];

    for (addrs, mut spec) in patterns::multi_search_ref(text_specs.iter().map(|x| &x.pattern), exe.text(), exe.text_offset_from_base(), &refs).iter().zip(text_specs) {
        if addrs.is_empty() && spec.segment == SegmentType::Any {
            rdata_specs.push(spec);
        } else {
            spec.add_addrs(addrs);
            let name = format_name_for_addr(spec.full_name.into());
            for addr in addrs {
                log::info!("{}: 0x{:X}", name, addr);
            }
            refs.insert(name, addrs.to_owned());
            all_specs.push(spec);
        }
    }
    
    for (addrs, mut spec) in patterns::multi_search_ref(rdata_specs.iter().map(|x| &x.pattern), exe.rdata(), exe.rdata_offset_from_base(), &refs).iter().zip(rdata_specs) {
        if addrs.is_empty() && spec.segment == SegmentType::Any {
            data_specs.push(spec);
        } else {
            spec.add_addrs(addrs);
            let name = format_name_for_addr(spec.full_name.into());
            for addr in addrs {
                log::info!("{}: 0x{:X}", name, addr);
            }
            refs.insert(name, addrs.to_owned());
            all_specs.push(spec);
        }
    }
    
    for (addrs, mut spec) in patterns::multi_search_ref(data_specs.iter().map(|x| &x.pattern), exe.data(), exe.data_offset_from_base(), &refs).iter().zip(data_specs) {
        spec.add_addrs(addrs);
        let name = format_name_for_addr(spec.full_name.into());
        for addr in addrs {
            log::info!("{}: 0x{:X}", name, addr);
        }
        refs.insert(name, addrs.to_owned());
        all_specs.push(spec);
    }

    for spec in all_specs {
        match spec.addrs[..] {
            [addr] => {
                let symbol = resolve_symbol(&spec, exe, addr)?;
                syms.push(symbol);
            },
            [] => {
                errs.push(SymbolError::NoMatches(spec.full_name));
                notf.push(FunctionSymbol::new(spec.name, spec.full_name, spec.spec_type.clone(), 0, spec.file_name, spec.needs_impl));
            },
            _ => {
                if let Some((n, max)) = spec.nth_entry_of {
                    match spec.addrs.get(n) {
                        Some(rva) if max == spec.addrs.len() || max == 0 => {
                            let symbol = resolve_symbol(&spec, exe, *rva)?;
                            syms.push(symbol);
                        }
                        Some(_) => {
                            errs.push(SymbolError::CountMismatch(spec.full_name, spec.addrs.len()));
                            notf.push(FunctionSymbol::new(spec.name, spec.full_name, spec.spec_type.clone(), 0, spec.file_name, spec.needs_impl));
                        }
                        None => {
                            errs.push(SymbolError::NotEnoughMatches(spec.full_name, spec.addrs.len()));
                            notf.push(FunctionSymbol::new(spec.name, spec.full_name, spec.spec_type.clone(), 0, spec.file_name, spec.needs_impl));
                        }
                    }
                } else {
                    errs.push(SymbolError::MoreThanOneMatch(spec.full_name, spec.addrs.len()));
                    notf.push(FunctionSymbol::new(spec.name, spec.full_name, spec.spec_type.clone(), 0, spec.file_name, spec.needs_impl));
                }
            },
        }
    }    
    Ok((syms, errs, notf))
}

fn resolve_symbol(spec: &SymbolSpec, data: &ExecutableData, rva: u64) -> Result<FunctionSymbol> {
    let res = match &spec.eval {
        Some(expr) => {
            expr.eval(&EvalContext::new(&spec.pattern, data, data.rel_offset(rva))?)? - data.image_base()
        }
        None => (rva as i64 - spec.offset.unwrap_or(0) as i64) as u64,
    };
    Ok(FunctionSymbol::new(
        spec.name,
        spec.full_name,
        spec.spec_type.clone(),
        res,
        spec.file_name,
        spec.needs_impl,
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSymbol {
    name: Ustr,
    full_name: Ustr,
    function_type: Type,
    rva: u64,
    file_name: Option<Ustr>,
    needs_impl: bool,
    addr_name: Ustr,
}

impl FunctionSymbol {
    fn new(
        name: Ustr,
        full_name: Ustr,
        function_type: Type,
        rva: u64,
        file_name: Option<Ustr>,
        needs_impl: bool,
    ) -> Self {
        Self {
            name,
            full_name,
            function_type,
            rva,
            file_name,
            needs_impl,
            addr_name: "".into(),
        }
    }

    // fn eq(&self, other: &FunctionSymbol) -> bool {
    //     self.full_name == other.full_name
    // }

    pub fn func_name(&self) -> &str {
        let v: Vec<&str> = self.full_name.split("::").collect();
        v[v.len() - 1]
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn dedup(&self, number: u32) -> Self {
        let mut n = self.clone();
        n.addr_name = format!("{}_{}", self.addr_name, number).into();
        n
    }

    pub fn context(&self) -> String {
        let mut v: Vec<&str> = self.full_name.split("::").collect();
        v.remove(v.len() - 1);
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

    pub fn addr_name(&self) -> &str {
        &self.addr_name
    }

    pub fn set_addr_name(&mut self, addr_name: Ustr) {
        self.addr_name = addr_name;
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
