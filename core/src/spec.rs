use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::str::FromStr;

use ustr::Ustr;

use crate::error::{Error, ParamError, Result};
use crate::eval::Expr;
use crate::patterns::Pattern;
use crate::types::{FunctionType, Type};

#[derive(Debug)]
pub struct FunctionSpec {
    pub name: Ustr,
    pub full_name: Ustr,
    pub spec_type: Type,
    pub pattern: Pattern,
    pub offset: Option<i64>,
    pub eval: Option<Expr>,
    pub nth_entry_of: Option<(usize, usize)>,
    pub file_name: Option<Ustr>,
    pub needs_impl: bool
}

impl FunctionSpec {
    pub fn new<'a, I>(name: Ustr, full_name: Ustr, spec_type: Type, comments: I, file_name: Option<Ustr>) -> Option<Result<Self>>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut params = HashMap::new();
        for comment in comments {
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

    fn from_params(
        name: Ustr,
        full_name: Ustr,
        spec_type: Type,
        mut params: HashMap<&str, &str>,
        file_name: Option<Ustr>
    ) -> Result<Self, ParamError> {
        let pattern = Pattern::parse(params.remove("pattern").ok_or(ParamError::MissingPattern)?)
            .map_err(|err| ParamError::ParseError("pattern", err))?;
        let offset = params
            .remove("offset")
            .map(|str| parse_from_str(str, "offset"))
            .transpose()?;
        let eval = params
            .remove("eval")
            .map(Expr::parse)
            .transpose()
            .map_err(|err| ParamError::ParseError("eval", err))?;
        let needs_impl = params.remove("noimpl").is_none();
        let nth_entry_of = params.remove("nth").map(parse_index_specifier).transpose()?;
        for key in params.keys() {
            log::warn!("{} unknown parameter: '{}'", full_name, key.deref().to_owned());
            // return Err(ParamError::UnknownParam(str.deref().to_owned()));
        }

        Ok(Self {
            name,
            full_name,
            spec_type,
            pattern,
            offset,
            eval,
            nth_entry_of,
            file_name,
            needs_impl
        })
    }
}

fn parse_typedef_comment(line: &str) -> Option<(&str, &str)> {
    let (key, val) = line
        .trim_start()
        .strip_prefix("///")
        .or_else(|| line.trim_start().strip_prefix("*"))?
        .trim_start()
        .strip_prefix('@')?
        .split_once(' ')?;

    Some((key, val.trim()))
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

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::eval::Expr;
    use crate::types::{Type, FunctionEnum};

    #[test]
    fn parse_valid_spec() {
        let spec_type = FunctionType::new(vec![], Type::Void, FunctionEnum::Typedef);
        let comment = [
            "/// @pattern E8 (fn:rel) 45 8B 86 70 01 00 00 33 C9 BA 05 00 00 00 C7 44 24 30 02 00 00 00",
            "/// @nth 5/24",
            "/// @offset 13",
            "/// @eval fn",
        ];
        let spec = FunctionSpec::new("test".into(), "test::test".into(), spec_type.into(), comment.into_iter());

        assert_matches!(
            spec,
            Some(Ok(FunctionSpec {
                nth_entry_of: Some((5, 24)),
                offset: Some(13),
                eval: Some(Expr::Ident(_)),
                ..
            }))
        )
    }
}
