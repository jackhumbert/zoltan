use std::fmt::Display;
use std::io::Write;

use itertools::Itertools;

use crate::error::Result;
use crate::symbols::FunctionSymbol;
use crate::types::{Type, TypeInfo};

const HEADER: &str = "\
// This file has been generated by zoltan (https://github.com/jac3km4/zoltan)
";

pub fn write_c_header<W: Write>(mut output: W, symbols: &[FunctionSymbol]) -> Result<()> {
    writeln!(output, "{}", HEADER)?;
    for symbol in symbols {
        // writeln!(
        //     output,
        //     "#define {}_ADDR 0x{:X}",
        //     symbol.name().to_uppercase(),
        //     symbol.rva()
        // )?;
        writeln!(
            output,
            "#define {}Addr 0x{:X}",
            symbol.name(),
            symbol.rva()
        )?;
    }

    Ok(())
}

pub fn write_rust_header<W: Write>(mut output: W, symbols: &[FunctionSymbol]) -> Result<()> {
    writeln!(output, "{}", HEADER)?;
    for symbol in symbols {
        writeln!(
            output,
            "const {}_ADDR: usize = 0x{:X};",
            symbol.name().to_uppercase(),
            symbol.rva()
        )?;
    }

    Ok(())
}

pub fn write_idc_funs<W: Write>(mut out: W, funs: &[FunctionSymbol]) -> Result<()> {
    writeln!(out)?;
    for fun in funs {
        writeln!(out, "// START_DECL FUNC {}", fun.rva())?;
        writeln!(
            out,
            r"typedef {};",
            Type::Function(fun.function_type()).name_with_id(fun.name())
        )?;
        writeln!(out, "// END_DECL")?;
    }

    Ok(())
}

pub fn write_idc_types<W: Write>(mut out: W, info: &TypeInfo) -> Result<()> {
    let pad = Pad(2);
    writeln!(out, "{}", HEADER)?;

    for (id, struc) in &info.structs {
        writeln!(out)?;
        writeln!(out, "// START_DECL HEADER")?;
        write!(out, "struct {id} ")?;
        // if let Some(base) = struc.base {
        //     write!(out, ": {base} ")?;
        // }
        writeln!(out, "{{}}")?;
        writeln!(out, "// END_DECL")?;

        if struc.has_virtual_methods(info) {
            writeln!(out)?;
            writeln!(out, "// START_DECL HEADER")?;
            write!(out, "struct {id}_vtbl ")?;
            // if let Some(base) = struc.base {
            //     write!(out, ": {base}_vtbl ")?;
            // }
            writeln!(out, "{{}}")?;
            writeln!(out, "// END_DECL")?;
        }
    }

    for id in info.unions.keys() {
        writeln!(out)?;
        writeln!(out, "// START_DECL HEADER")?;
        writeln!(out, "union {id} {{}}")?;
        writeln!(out, "// END_DECL")?;
    }

    for (id, ty) in &info.enums {
        writeln!(out)?;
        writeln!(out, "// START_DECL TYPE")?;
        write!(out, "enum {id}")?;
        if let Some(underlying) = &ty.underlying_type {
            write!(out, ": {}", underlying.name())?;
        }
        writeln!(out, " {{")?;
        for m in &ty.members {
            let name = format!("{}_{}", id.to_string().replace("::", "_"), m.name);
            writeln!(out, "{pad}{} = {},", name, m.value)?;
        }
        writeln!(out, "}}")?;
        writeln!(out, "// END_DECL")?;
    }

    for (id, struc) in &info.structs {
        let safe_id = id.to_string().replace("::", "_").replace("~", "_");

        if struc.members.len() != 0 || struc.base.len() > 0 || (struc.has_direct_virtual_methods() && !struc.has_indirect_virtual_methods(info)) {
            writeln!(out)?;
            writeln!(out, "// START_DECL TYPE")?;
            write!(out, "struct ")?;
            if struc.has_virtual_methods(info) {
                write!(out, "__cppobj ")?;
            }
            write!(out, "{id} ")?;
            if struc.base.len() > 0 {
                let base = itertools::join(&struc.base, ", ");
                write!(out, ": {base}")?;
            }
            writeln!(out, " {{")?;

            if struc.has_direct_virtual_methods() && !struc.has_indirect_virtual_methods(info) {
                writeln!(out, r#"{pad}{id}_vtbl *__vftable;"#)?;
            }

            for (i, m) in struc.members.iter().enumerate() {
                if m.is_bitfield {
                    let bit_size = struc
                        .members
                        .get(i + 1)
                        .and_then(|m| m.bit_offset)
                        .zip(m.bit_offset)
                        .map(|(a, b)| a - b)
                        .unwrap_or(1);
                    writeln!(out, "{pad}{}: {bit_size};", m.typ.name_with_id(&m.name))?;
                } else {
                    writeln!(out, "{pad}{};", m.typ.name_with_id(&m.name))?;
                }
            }
            writeln!(out, "}}")?;
            writeln!(out, "// END_DECL")?;
        }

        if struc.rva != 0 {
            writeln!(out)?;
            writeln!(out, "// START_DECL VTABLE {}", struc.rva)?;
            writeln!(out, "{safe_id}")?;
            writeln!(out, "// END_DECL")?;
        }

        if struc.has_virtual_methods(info) && (struc.virtual_methods.len() > 0 || struc.base.len() > 0)  {
            writeln!(out)?;
            writeln!(out, "// START_DECL TYPE")?;
            write!(out, "struct {id}_vtbl ")?;
            // if struc.base.len() > 0 {
                // let base = itertools::join(&struc.base, ", ");
               if let Some(base) = struc.base.first() {
                write!(out, ": {base}_vtbl")?;
               }
            // }
            writeln!(out, "{{")?;
            for m in &struc.virtual_methods {
                if m.typ.params.is_empty() {
                    writeln!(
                        out,
                        "{pad}{} (*{})({id} *__hidden this);",
                        m.typ.return_type.name(),
                        m.name,
                    )?;
                } else {
                    writeln!(
                        out,
                        "{pad}{} (*{})({id} *__hidden this, {});",
                        m.typ.return_type.name(),
                        m.name,
                        m.typ.params.iter().map(Type::name).format(", "),
                    )?;
                }
            }
            writeln!(out, "}}")?;
            writeln!(out, "// END_DECL")?;

            if struc.rva != 0 {
                for m in &struc.overridden_virtual_methods {
                    let rva = struc.rva + m.offset;
                    // let rva = m.offset;
                    writeln!(out)?;
                    writeln!(out, "// START_DECL VFUNC {rva}")?;
                    let safe_name = m.name.replace("~", "_");
                    if m.typ.params.is_empty() {
                        writeln!(
                            out,
                            "typedef {} {safe_id}_{}({id} *__hidden this);",
                            m.typ.return_type.name(),
                            safe_name,
                        )?;
                    } else {
                        writeln!(
                            out,
                            "typedef {} {safe_id}_{}({id} *__hidden this, {});",
                            m.typ.return_type.name(),
                            safe_name,
                            m.typ.params.iter().map(Type::name).format(", "),
                        )?;
                    }
                    writeln!(out, "// END_DECL")?;
                }
                for m in &struc.virtual_methods {
                    let rva = struc.rva + m.offset;
                    // let rva = m.offset;
                    writeln!(out)?;
                    writeln!(out, "// START_DECL VFUNC {rva}")?;
                    let safe_name = m.name.replace("~", "_");
                    if m.typ.params.is_empty() {
                        writeln!(
                            out,
                            "typedef {} {safe_id}_{}({id} *__hidden this);",
                            m.typ.return_type.name(),
                            safe_name,
                        )?;
                    } else {
                        writeln!(
                            out,
                            "typedef {} {safe_id}_{}({id} *__hidden this, {});",
                            m.typ.return_type.name(),
                            safe_name,
                            m.typ.params.iter().map(Type::name).format(", "),
                        )?;
                    }
                    writeln!(out, "// END_DECL")?;
                }
            }
        }
    }

    for (id, en) in &info.unions {
        writeln!(out)?;
        writeln!(out, "// START_DECL TYPE")?;
        writeln!(out, "union {id} {{")?;
        for m in &en.members {
            writeln!(out, "{pad}{};", m.typ.name_with_id(&m.name))?;
        }
        writeln!(out, "}}")?;
        writeln!(out, "// END_DECL")?;
    }

    Ok(())
}

struct Pad(usize);

impl Display for Pad {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{0:1$}", ' ', self.0)
    }
}
