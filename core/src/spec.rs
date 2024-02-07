use std::collections::HashMap;
use std::str::FromStr;

use strum_macros::EnumString;
use ustr::Ustr;

use crate::error::{Error, ParamError, Result};
use crate::eval::Expr;
use crate::patterns::Pattern;
use crate::types::Type;

#[derive(Debug, Copy, Clone, EnumString, PartialEq)]
pub enum SegmentType {
    #[strum(serialize = "any")]
    Any,
    #[strum(serialize = "text", serialize = "t")]
    Text,
    #[strum(serialize = "idata", serialize = "i")]
    IData,
    #[strum(serialize = "rdata", serialize = "r")]
    RData,
    #[strum(serialize = "data", serialize = "d")]
    Data,
    #[strum(serialize = "pdata", serialize = "p")]
    PData,
}

#[derive(Debug)]
pub struct SymbolSpec {
    pub name: Ustr,
    pub full_name: Ustr,
    pub spec_type: Type,
    pub pattern: Option<Pattern>,
    pub offset: Option<i64>,
    pub eval: Option<Expr>,
    pub nth_entry_of: Option<(usize, usize)>,
    pub file_name: Option<Ustr>,
    pub needs_impl: bool,
    pub segment: SegmentType,
    pub addrs: Vec<u64>,
    pub hash: Option<u64>,
}

impl SymbolSpec {
    pub fn new<'a, I>(
        name: Ustr,
        full_name: Ustr,
        spec_type: Type,
        comments: I,
        file_name: Option<Ustr>,
    ) -> Option<Result<Self>>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut params = HashMap::new();
        let mut stripped_comments = vec![];
        for comment in comments {
            let mut stripped = comment
                .trim_start()
                .strip_prefix("///")
                .or_else(|| comment.trim_start().strip_prefix("*"))?
                .trim_start()
                .to_string();
            if stripped.contains("//") {
                stripped = stripped
                    .split("//")
                    .collect::<Vec<&str>>()
                    .first()
                    .unwrap()
                    .to_string();
            }
            if stripped.starts_with("@") {
                stripped_comments.push(stripped);
            } else {
                if let Some(last) = stripped_comments.last_mut() {
                    last.push(' ');
                    last.push_str(stripped.trim());
                }
            }
        }
        for comment in stripped_comments.iter().map(String::as_str) {
            if let Some((key, val)) = parse_typedef_comment(comment) {
                params.insert(key, val);
            }
        }
        if params.is_empty() {
            None
        } else {
            let spec = Self::from_params(name, full_name, spec_type, params, file_name)
                .map_err(|err| Error::TypedefParamError(name, err));
            Some(spec)
        }
    }

    pub fn add_addrs(&mut self, addrs: &Vec<u64>) {
        for addr in addrs {
            self.addrs.push(*addr);
        }
    }

    fn from_params(
        name: Ustr,
        full_name: Ustr,
        spec_type: Type,
        mut params: HashMap<&str, &str>,
        file_name: Option<Ustr>,
    ) -> Result<Self, ParamError> {
        let pattern = params
            .remove("pattern")
            .map(Pattern::parse)
            .transpose()
            .map_err(|err| ParamError::ParseError("pattern", err))?;
        let redhash = params
            .remove("hash")
            .map(hash::parse)
            .transpose()
            .map_err(|err| ParamError::ParseError("hash", err))?;
        let (hash, hash_segment) = redhash.map(|r| (r.hash, r.segment)).unzip();
        let mut segment = hash_segment.unwrap_or(SegmentType::Any);
        let offset = params
            .remove("offset")
            .map(|str| parse_offset_from_str(str, "offset"))
            .transpose()?;
        let eval = params
            .remove("eval")
            .map(Expr::parse)
            .transpose()
            .map_err(|err| ParamError::ParseError("eval", err))?;
        let needs_impl = params.remove("noimpl").is_none();
        let nth_entry_of = params.remove("nth").map(parse_index_specifier).transpose()?;
        if params.contains_key("segment") {
            segment = SegmentType::from_str(params.remove("segment").unwrap_or_default())
                .unwrap_or(SegmentType::Any)
        };
        // for key in params.keys() {
        // log::warn!("{} unknown parameter: '{}'", full_name, key.deref().to_owned());
        // return Err(ParamError::UnknownParam(str.deref().to_owned()));
        // }
        let addrs = vec![];

        Ok(Self {
            name,
            full_name,
            spec_type,
            pattern,
            offset,
            eval,
            nth_entry_of,
            file_name,
            needs_impl,
            segment,
            addrs,
            hash,
        })
    }
}

#[derive(Debug, Clone)]
pub struct RedHash {
    hash: u64,
    segment: SegmentType,
}

peg::parser! {
    grammar hash() for str {
        rule _() =
            quiet!{[' ' | '\t' | '\n' | '\\']*}
        rule number() -> u64
            = "0x" n:$(['0'..='9' | 'A'..='F']+) {? u64::from_str_radix(n, 16).or(Err("return number")) }
            / n:$(['0'..='9']+) {? u64::from_str_radix(n, 10).or(Err("return number")) }
        rule segment() -> String
            = id:$(['a'..='z' | 'A'..='Z']*) { id.to_owned() }
        pub rule parse() ->  RedHash
            = _ hash:number() _ ":" _ seg:segment() _ { RedHash { hash, segment: SegmentType::from_str(&seg).unwrap_or(SegmentType::Text) } }
            / _ hash:number() _ { RedHash { hash, segment: SegmentType::Text } }
    }
}

fn parse_typedef_comment(line: &str) -> Option<(&str, &str)> {
    let (key, val) = line.strip_prefix('@')?.split_once(' ')?;

    Some((key.clone(), val.trim().clone()))
}

fn parse_index_specifier(str: &str) -> Result<(usize, usize), ParamError> {
    let (n, max) = str
        .split_once('/')
        .ok_or_else(|| ParamError::InvalidParam("nth", "invalid format".to_string()))?;
    Ok((
        parse_from_str(n.trim(), "nth")?,
        parse_from_str(max.trim(), "nth")?,
    ))
}

fn parse_from_str<F: FromStr>(str: &str, field: &'static str) -> Result<F, ParamError>
where
    F::Err: std::error::Error,
{
    str.parse()
        .map_err(|err: F::Err| ParamError::InvalidParam(field, err.to_string()))
}

fn parse_offset_from_str(str: &str, field: &'static str) -> Result<i64, ParamError> {
    str.parse()
        .or_else(|s| {
            let without_prefix = str.replace("0x", "");
            i64::from_str_radix(&without_prefix, 16)
        })
        .map_err(|err| ParamError::InvalidParam(field, err.to_string()))
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::eval::Expr;
    use crate::types::{FunctionEnum, FunctionType, Type};

    #[test]
    fn parse_valid_spec() {
        let spec_type = FunctionType::new(vec![], Type::Void, FunctionEnum::Typedef);
        let comment = [
            "/// @pattern E8 (fn:rel) 45 8B 86 70 01 00 00 33 C9 BA 05 00 00 00 C7 44 24 30 02 00 00 00",
            "/// @nth 5/24",
            "/// @offset 13",
            "/// @eval fn",
            "/// @segment rdata",
        ];
        let spec = SymbolSpec::new(
            "test".into(),
            "test::test".into(),
            Type::Function(spec_type.into()),
            comment.into_iter(),
            None,
        );

        assert_matches!(
            spec,
            Some(Ok(SymbolSpec {
                nth_entry_of: Some((5, 24)),
                offset: Some(13),
                eval: Some(Expr::Ident(_)),
                segment: SegmentType::RData,
                ..
            }))
        )
    }
    #[test]
    fn parse_valid_hex_spec() {
        let spec_type = FunctionType::new(vec![], Type::Void, FunctionEnum::Typedef);
        let comment = [
            "/// @pattern E8 (fn:rel) 45 8B 86 70 01 00 00 33 C9 BA 05 00 00 00 C7 44 24 30 02 00 00 00",
            "/// @nth 5/24",
            "/// @offset -0x13",
            "/// @eval fn",
        ];
        let spec = SymbolSpec::new(
            "test".into(),
            "test::test".into(),
            Type::Function(spec_type.into()),
            comment.into_iter(),
            None,
        );

        assert_matches!(
            spec,
            Some(Ok(SymbolSpec {
                nth_entry_of: Some((5, 24)),
                offset: Some(-0x13),
                eval: Some(Expr::Ident(_)),
                ..
            }))
        )
    }
    #[test]
    fn parse_vft_decl() {
        let spec_type = FunctionType::new(vec![], Type::Void, FunctionEnum::Typedef);
        let comment = [
            "/// @pattern",
            "///     /vft                                        // GetNativeType",
            "///     /vft                                        // GetType",
            "///     /vft                                        // GetAllocator",
            "///     /vft(ISerializable_dstr)                    // ~ISerializable",
            "///     /vft(null)                                  // sub_20",
            "///     /vft(null)                                  // sub_28",
            "///     /vft                                        // sub_30",
            "///     /vft(null)                                  // sub_38",
            "///     /vft(ISerializable_OnSerialize)             // sub_40",
            "///     /vft(ISerializable_OnSerializeToText)       // sub_48",
            "///     /vft(ISerializable_OnSerializeFromText)     // sub_50",
            "///     /vft(ret 0)                                 // sub_58",
            "///     /vft(ret(0))                                // sub_60",
            "///     /vft(ret(0))                                // sub_68",
            "/// @segment rdata",
        ];
        let spec = SymbolSpec::new(
            "test".into(),
            "test::test".into(),
            Type::Function(spec_type.into()),
            comment.into_iter(),
            None,
        );

        assert_matches!(
            spec,
            Some(Ok(SymbolSpec {
                segment: SegmentType::RData,
                ..
            }))
        );
        assert_matches!(spec.unwrap().unwrap().pattern.unwrap().parts(), &[
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Any
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Any
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Any
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Ref(_)
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Null
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Null
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Any
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Null
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Ref(_)
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Ref(_)
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Ref(_)
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Return(0)
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Return(0)
            )),
            crate::patterns::PatItem::ByteCode(crate::patterns::ByteCode::Vft(
                crate::patterns::VftType::Return(0)
            )),
        ]);
    }
}
