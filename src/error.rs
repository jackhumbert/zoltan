use std::collections::VecDeque;
use std::io;

use peg::str::LineCol;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid parameter in '{0}': {1}")]
    TypedefParamError(String, ParamError),
    #[error("invalid rdata access at {0}")]
    InvalidAccess(usize),
    #[error("unresolved name {0}")]
    UnresolvedName(String),
    #[error("compile errors:\n{0}")]
    CompileError(String),
    #[error("object file error: {0}")]
    ObjectError(#[from] object::Error),
    #[error("DWARF error: {0}")]
    DwarfError(#[from] gimli::write::Error),
    #[error("I/O error: {0}")]
    IoError(#[from] io::Error),
    #[error("missing {0} section")]
    MissingSection(&'static str),
    #[error("{0}")]
    OtherError(#[from] Box<dyn std::error::Error>),
}

impl Error {
    pub fn from_compile_errors(errs: VecDeque<saltwater::CompileError>, files: &saltwater::Files) -> Self {
        let message = errs
            .iter()
            .map(|err| {
                let loc = files
                    .location(err.location.file, err.location.span.start)
                    .unwrap();
                format!("at {}:{}: {}", loc.line.0 + 1, loc.column.0 + 1, err.data)
            })
            .collect::<Vec<_>>()
            .join("\n");

        Self::CompileError(message)
    }
}

#[derive(Debug, Error)]
pub enum SymbolError {
    #[error("too many matches for {0} ({1})")]
    MoreThanOneMatch(String, usize),
    #[error("no matches for {0}")]
    NoMatches(String),
    #[error("not enough matches for {0} ({1})")]
    NotEnoughMatches(String, usize),
    #[error("count mismatch for {0} ({1})")]
    CountMismatch(String, usize),
}

#[derive(Debug, Error)]
pub enum ParamError {
    #[error("invalid parameter '{0}': {1}")]
    InvalidParam(&'static str, String),
    #[error("unknown parameter '{0}'")]
    UnknownParam(String),
    #[error("missing 'pattern' parameter")]
    MissingPattern,
    #[error("parse error in '{0}': {1}")]
    ParseError(&'static str, peg::error::ParseError<LineCol>),
}
