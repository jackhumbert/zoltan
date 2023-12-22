use std::collections::HashMap;

use aho_corasick::AhoCorasick;
use enum_as_inner::EnumAsInner;

use crate::{symbols::FunctionSymbol, codegen::format_name_for_addr};

#[derive(Debug, EnumAsInner)]
pub enum PatItem {
    Byte(u8),
    Any,
    Group(String, VarType),
    ByteCode(ByteCode)
}

impl PatItem {
    #[inline]
    fn size(&self) -> usize {
        match self {
            PatItem::Byte(_) => 1,
            PatItem::Any => 1,
            PatItem::Group(_, VarType::Rel) => 4,
            PatItem::ByteCode(ByteCode::Vft(_)) => 8,
            PatItem::ByteCode(ByteCode::Lea(_)) => 7,
            PatItem::ByteCode(ByteCode::Mov(MovType::U8(_))) => 2,
            PatItem::ByteCode(ByteCode::Mov(MovType::U32(_))) => 5,
            PatItem::ByteCode(ByteCode::Mov(MovType::Ref(_))) => 7,
            PatItem::ByteCode(ByteCode::Call(_)) => 5,
            PatItem::ByteCode(ByteCode::Retn) => 1,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            PatItem::Byte(b) => vec![*b],
            PatItem::Any => vec![],
            PatItem::Group(_, VarType::Rel) => vec![],
            PatItem::ByteCode(ByteCode::Vft(VftType::PureCall)) => vec![0xC8, 0xA0, 0x7D, 0x41, 0x01, 0x00, 0x00, 0x00],
            PatItem::ByteCode(ByteCode::Vft(_)) => vec![],
            PatItem::ByteCode(ByteCode::Lea(_)) => vec![],
            PatItem::ByteCode(ByteCode::Mov(MovType::U8(b))) => vec![0xB0, *b],
            PatItem::ByteCode(ByteCode::Mov(MovType::U32(b))) => {
                let mut v = u32::to_ne_bytes(*b).to_vec();
                v.insert(0, 0xB8);
                v
            },
            PatItem::ByteCode(ByteCode::Mov(MovType::Ref(_))) => vec![],
            PatItem::ByteCode(ByteCode::Call(_)) => vec![],
            PatItem::ByteCode(ByteCode::Retn) => vec![0xC3],
        }
    }

    pub fn to_bytes_ref(&self, refs: &HashMap<String, Vec<u64>>) -> Vec<u8> {
        match self {
            PatItem::ByteCode(ByteCode::Vft(VftType::Ref(name))) => {
                if let Some([addr]) = refs.get(name).map(|vec| &vec[..]) {
                    u64::to_ne_bytes(addr + 0x0140000000).into() 
                } else {
                    vec![]
                }
            },
            _ => self.to_bytes(),
        }
    }
    
    pub fn does_match(&self, bytes: &mut std::iter::Enumerate<std::slice::Iter<'_, u8>>, start: u64) -> bool {
        match self {
            PatItem::Byte(expected) => {
                match bytes.next() {
                    Some((_, &byte)) => {
                        if byte != *expected {
                            return false;
                        }
                     },
                    _ => { return false; }
                }
            },
            PatItem::Group(_, var_type) => {
                match var_type {
                    VarType::Rel => if bytes.advance_by(self.size()).is_err() {
                        return false;
                    },
                }
            },
            PatItem::ByteCode(ByteCode::Vft(VftType::PureCall)) => {
                let comp_bytes: [u8; 8] = [0xC8, 0xA0, 0x7D, 0x41, 0x01, 0x00, 0x00, 0x00];
                for comp_byte in comp_bytes {
                    match bytes.next() {
                        Some((_, &byte)) => {
                            if byte != comp_byte {
                                return false;
                            }
                         },
                        _ => { return false; }
                    }
                }
            },
            PatItem::ByteCode(ByteCode::Retn) => { return matches!(bytes.next(), Some((_, 0xC3))) },
            PatItem::Any | PatItem::ByteCode(ByteCode::Vft(VftType::Any)) => if bytes.advance_by(self.size()).is_err() {
                return false;
            },
            _ => { return false; }
        }
        true
    }

    pub fn does_match_ref(&self, bytes: &mut std::iter::Enumerate<std::slice::Iter<'_, u8>>, start: u64, refs: &HashMap<String, Vec<u64>>) -> bool {
        match self {
            PatItem::ByteCode(ByteCode::Vft(VftType::Null)) => {
                if let Some(addrs) = refs.get("null") {
                    let mut addr_bytes: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
                    for i in 0..self.size() {
                        if let Some((_, &b)) = bytes.next() {
                            addr_bytes[i] = b;
                        } else {
                            return false;
                        }
                    }
                    let byte_addr = u64::from_ne_bytes(addr_bytes);
                    for &addr in addrs {
                        if addr == byte_addr - 0x0140000000 {
                            return true;
                        }
                    }
                }
                return false;
            },
            PatItem::ByteCode(ByteCode::Vft(VftType::Return(value))) => {
                if let Some(addrs) = refs.get(format!("ret{}", value).as_str()) {
                    let mut addr_bytes: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
                    for i in 0..self.size() {
                        if let Some((_, &b)) = bytes.next() {
                            addr_bytes[i] = b;
                        } else {
                            return false;
                        }
                    }
                    let byte_addr = u64::from_ne_bytes(addr_bytes);
                    for &addr in addrs {
                        if addr == byte_addr - 0x0140000000 {
                            return true;
                        }
                    }
                }
                return false;
            },
            PatItem::ByteCode(ByteCode::Vft(VftType::Ref(name))) => {
                if let Some(addrs) = refs.get(name) {
                    let mut addr_bytes: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
                    for i in 0..self.size() {
                        if let Some((_, &b)) = bytes.next() {
                            addr_bytes[i] = b;
                        } else {
                            return false;
                        }
                    }
                    let byte_addr = u64::from_ne_bytes(addr_bytes);
                    for &addr in addrs {
                        if addr == byte_addr - 0x0140000000 {
                            return true;
                        }
                    }
                }
                return false;
            },
            PatItem::ByteCode(ByteCode::Mov(MovType::Ref(name))) => {
                if let Some(addrs) = refs.get(name) {
                    let comp_bytes = vec![0x48, 0x8B, 0x05];
                    let mut offset = 0;
                    for comp_byte in comp_bytes {
                        match bytes.next() {
                            Some((_, &byte)) => {
                                if byte != comp_byte {
                                    return false;
                                }
                             },
                            _ => { return false; }
                        }
                    }
                    let mut addr_bytes = vec![];
                    for _ in 0..4 {
                        if let Some((o, &b)) = bytes.next() {
                           addr_bytes.push(b);
                           offset = o;
                        }
                    }
                    let byte_addr = i32::from_ne_bytes(addr_bytes.try_into().unwrap());
                    for addr in addrs {
                        if ((start + 1 + offset as u64) as i64 + byte_addr as i64) as u64 == *addr  {
                            return true;
                        }
                    }
                }
                return false;
            },
            PatItem::ByteCode(ByteCode::Lea(LeaType::Ref(name))) => {
                if let Some(addrs) = refs.get(name) {
                    let comp_bytes = vec![0x48, 0x8D, 0x05];
                    let mut offset = 0;
                    for comp_byte in comp_bytes {
                        match bytes.next() {
                            Some((_, &byte)) => {
                                if byte != comp_byte {
                                    return false;
                                }
                             },
                            _ => { return false; }
                        }
                    }
                    let mut addr_bytes = vec![];
                    for _ in 0..4 {
                        if let Some((o, &b)) = bytes.next() {
                           addr_bytes.push(b);
                           offset = o;
                        }
                    }
                    let byte_addr = i32::from_ne_bytes(addr_bytes.try_into().unwrap());
                    for addr in addrs {
                        if ((start + 1 + offset as u64) as i64 + byte_addr as i64) as u64 == *addr  {
                            return true;
                        }
                    }
                }
                return false;
            },
            PatItem::ByteCode(ByteCode::Lea(LeaType::Str(name))) => {
                if let Some(addrs) = refs.get(name) {
                    let comp_bytes = vec![0x48, 0x8D, 0x15];
                    let mut offset = 0;
                    for comp_byte in comp_bytes {
                        match bytes.next() {
                            Some((_, &byte)) => {
                                if byte != comp_byte {
                                    return false;
                                }
                             },
                            _ => { return false; }
                        }
                    }
                    let mut addr_bytes = vec![];
                    for _ in 0..4 {
                        if let Some((o, &b)) = bytes.next() {
                           addr_bytes.push(b);
                           offset = o;
                        }
                    }
                    let byte_addr = i32::from_ne_bytes(addr_bytes.try_into().unwrap());
                    for addr in addrs {
                        if ((start + 1 + offset as u64) as i64 + byte_addr as i64) as u64 == *addr  {
                            return true;
                        }
                    }
                }
                return false;
            },
            PatItem::ByteCode(ByteCode::Call(name)) => {
                if let Some(addrs) = refs.get(name) {
                    let comp_bytes = vec![0xE8];
                    let mut offset = 0;
                    for comp_byte in comp_bytes {
                        match bytes.next() {
                            Some((_, &byte)) => {
                                if byte != comp_byte {
                                    return false;
                                }
                             },
                            _ => { return false; }
                        }
                    }
                    let mut addr_bytes = vec![];
                    for _ in 0..4 {
                        if let Some((o, &b)) = bytes.next() {
                           addr_bytes.push(b);
                           offset = o;
                        }
                    }
                    let byte_addr = i32::from_ne_bytes(addr_bytes.try_into().unwrap());
                    for addr in addrs {
                        if ((start + 1 + offset as u64) as i64 + byte_addr as i64) as u64 == *addr  {
                            return true;
                        }
                    }
                }
                return false;
            },
            _ => { return self.does_match(bytes, start); },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VarType {
    Rel,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum VftType {
    Any,
    Bytes(Vec<u8>),
    Ref(String),
    PureCall,
    Null,
    Return(u64),
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LeaType {
    Ref(String),
    Str(String),
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum MovType {
    U8(u8),
    U32(u32),
    Ref(String)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ByteCode {
    Vft(VftType),
    Lea(LeaType),
    Mov(MovType),
    Call(String),
    Retn,
}

#[derive(Debug)]
pub struct Pattern {
    parts: Vec<PatItem>,
    size: usize,
}

impl Pattern {
    #[inline]
    fn new(parts: Vec<PatItem>) -> Self {
        Self {
            size: parts.iter().map(PatItem::size).sum(),
            parts,
        }
    }

    pub fn parse(str: &str) -> Result<Self, peg::error::ParseError<peg::str::LineCol>> {
        pattern::pattern(str)
    }

    #[inline]
    pub fn parts(&self) -> &[PatItem] {
        &self.parts
    }

    #[inline]
    fn size(&self) -> usize {
        self.size
    }

    pub fn groups(&self) -> impl Iterator<Item = (&str, VarType, usize)> {
        self.parts
            .iter()
            .scan(0usize, |offset, it| {
                let pos = *offset;
                *offset += it.size();
                Some((it, pos))
            })
            .filter_map(|(it, offset)| it.as_group().map(|(key, typ)| (key.as_str(), *typ, offset)))
    }

    fn does_match(&self, bytes: &[u8], offset: u64) -> bool {
        let mut bytes = bytes.iter().enumerate();
        for pat in self.parts() {
            if !pat.does_match(&mut bytes, offset) {
                return false
            }
        }
        true
    }

    fn does_match_ref(&self, bytes: &[u8], offset: u64, refs: &HashMap<String, Vec<u64>>) -> bool {
        let mut bytes = bytes.iter().enumerate();
        for pat in self.parts() {
            if !pat.does_match_ref(&mut bytes, offset, refs) {
                return false
            }
        }
        true
    }

    fn longest_byte_sequence(&self) -> &[PatItem] {
        self.parts()
            .group_by(|a, b| !a.to_bytes().is_empty() && !b.to_bytes().is_empty())
            .max_by_key(|parts| parts.len())
            .unwrap_or_default()
    }

    fn longest_byte_sequence_ref(&self, refs: &HashMap<String, Vec<u64>>) -> &[PatItem] {
        self.parts()
            .group_by(|a, b| !a.to_bytes_ref(refs).is_empty() && !b.to_bytes_ref(refs).is_empty())
            .max_by_key(|parts| parts.len())
            .unwrap_or_default()
    }
}

peg::parser! {
    grammar pattern() for str {
        rule _() =
            quiet!{[' ' | '\t' | '\n' | '\\']*}
        rule byte() -> u8
            = n:$(['0'..='9' | 'A'..='F']*<2>) {? u8::from_str_radix(n, 16).or(Err("byte")) }
        rule any()
            = "?"
        rule ident() -> String
            = id:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { id.to_owned() }
        rule var_type() -> VarType
            = "rel" { VarType::Rel }
        rule number() -> u64
            = "0x" n:$(['0'..='9' | 'A'..='F']+) {? u64::from_str_radix(n, 16).or(Err("return number")) }
            / n:$(['0'..='9']+) {? u64::from_str_radix(n, 10).or(Err("return number")) }
        rule ret() -> u64
            = "(" _ n:number() _ ")" { n }
            / _ n:number() _ { n }
        rule vft() -> VftType
            = "pure" { VftType::PureCall }
            / "null" { VftType::Null }
            / "ret" ret:ret() { VftType::Return(ret) }
            // / b:byte() ++ _ { VftType::Bytes(b) }
            / id:ident() { VftType::Ref(id) }
        rule byte_code() ->  ByteCode
            = "vft(" _ v:vft() _ ")" { ByteCode::Vft(v) }
            / "vft" { ByteCode::Vft(VftType::Any) }
            / "lea(" _ id:ident() _ ")" { ByteCode::Lea(LeaType::Ref(id)) }
            / "lea(\"" str:$([^'"']+) "\")" { ByteCode::Lea(LeaType::Str(str.to_owned())) }
            / "mov(" _ id:ident() _ ")" { ByteCode::Mov(MovType::Ref(id)) }
            / "call(" _ id:ident() _ ")" { ByteCode::Call(id) }
            / "retn" { ByteCode::Retn }
        rule item() -> PatItem
            = n:byte() { PatItem::Byte(n) }
            / any() { PatItem::Any }
            / "(" _ id:ident() _ ":" _ typ:var_type() _ ")" { PatItem::Group(id, typ) }
            / "/" byte_code:byte_code() { PatItem::ByteCode(byte_code) }
        pub rule pattern() -> Pattern
            = items:item() ** _ { Pattern::new(items) }
    }
}

pub fn single_search(sequences: Vec<Vec<u8>>, haystack: &[u8]) -> Vec<u64> {
    let ac = AhoCorasick::new(&sequences);
    let mut matches = vec![];

    for mat in ac.find_overlapping_iter(haystack) {
        matches.push(mat.start() as u64);
    }
    matches
}

pub fn multi_search<'a, I>(patterns: I, haystack: &[u8], segment_start: u64) -> Vec<Vec<u64>>
where
    I: IntoIterator<Item = &'a Pattern>,
{
    let mut items = vec![];
    let mut sequences: Vec<Vec<u8>> = vec![];

    for pat in patterns {
        let has_byte_code = !pat.parts.iter().filter(|x| match x {
            PatItem::ByteCode(_) => true,
            _ => false
        }).collect::<Vec<&PatItem>>().is_empty();
        if !has_byte_code {
            let seq = pat.longest_byte_sequence();
            let start = offset_from(pat.parts(), seq);
            let offset: usize = pat.parts[0..start].iter().map(PatItem::size).sum();
            items.push((pat, offset));
            // sequences.push(seq.iter().filter_map(PatItem::as_byte).cloned().collect());
            sequences.push(seq.iter().flat_map(|x| {
                let bytes = x.to_bytes();
                if bytes.is_empty() {
                    None
                } else {
                    Some(bytes.as_slice().to_owned())
                }
            }).flatten().collect());
        } else {
            items.push((pat, 0));
            sequences.push(vec![]);
        }
    }

    let ac = AhoCorasick::new(&sequences);
    let mut matches: Vec<Vec<u64>> = vec![];

    for mat in ac.find_overlapping_iter(haystack) {
        let (pat, offset) = items[mat.pattern()];
        if mat.start() > offset && (mat.start() - offset + pat.size()) < haystack.len() {
            let start = mat.start() - offset;
            let slice = &haystack[start..start + pat.size()];

            if pat.does_match(slice, start as u64) {
                // let mat = Match {
                //     pattern: mat.pattern(),
                //     rva: start as u64 - segment_start,
                // };
                // matches.push(mat);
                matches[mat.pattern()].push(start as u64 - segment_start);
            }
        }
    }
    matches
}

pub fn multi_search_ref<'a, I>(patterns: I, haystack: &[u8], segment_start: u64, refs: &HashMap<String, Vec<u64>>) -> Vec<Vec<u64>>
where
    I: IntoIterator<Item = &'a Pattern>,
{
    let mut items = vec![];
    let mut sequences: Vec<Vec<u8>> = vec![];
    let mut matches: Vec<Vec<u64>> = vec![];

    for pat in patterns {
        // if !pat.parts.iter().filter(|x| match x {
        //     PatItem::Group(_, VarType::Ref) => true,
        //     _ => false
        // }).collect::<Vec<&PatItem>>().is_empty() {
        let seq = pat.longest_byte_sequence_ref(refs);
        let start = offset_from(pat.parts(), seq);
        let offset: usize = pat.parts[0..start].iter().map(PatItem::size).sum();
        items.push((pat, offset));
        sequences.push(seq.iter().flat_map(|x| {
            let bytes = x.to_bytes_ref(refs);
            if bytes.is_empty() {
                None
            } else {
                Some(bytes.as_slice().to_owned())
            }
        }).flatten().collect());
        // }
        matches.push(vec![]);
    }

    let ac = AhoCorasick::new(&sequences);

    for mat in ac.find_overlapping_iter(haystack) {
        let (pat, offset) = items[mat.pattern()];
        if mat.start() > offset && (mat.start() - offset + pat.size()) < haystack.len() {
            let start = mat.start() - offset;
            let slice = &haystack[start..start + pat.size()];

            if pat.does_match_ref(slice, segment_start + start as u64, refs) {
                // let mat = Match {
                //     pattern: mat.pattern(),
                //     rva: start as u64,
                // };
                // matches.push(mat);
                matches[mat.pattern()].push(start as u64 + segment_start);
            }
        }
    }
    matches
}

#[derive(Debug)]
pub struct Match {
    pub pattern: usize,
    pub rva: u64,
}

/// Returns the offset of `other` into `slice`.
#[inline]
fn offset_from<T>(slice: &[T], other: &[T]) -> usize {
    ((other.as_ptr() as usize) - (slice.as_ptr() as usize)) / std::mem::size_of::<T>()
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn parse_valid_patterns() {
        let pat = Pattern::parse("8B 0D ? ? BA 10").unwrap();
        assert_matches!(pat.parts(), &[
            PatItem::Byte(0x8B),
            PatItem::Byte(0x0D),
            PatItem::Any,
            PatItem::Any,
            PatItem::Byte(0xBA),
            PatItem::Byte(0x10),
        ]);

        let pat = Pattern::parse("8BF9E8??").unwrap();
        assert_matches!(pat.parts(), &[
            PatItem::Byte(0x8B),
            PatItem::Byte(0xF9),
            PatItem::Byte(0xe8),
            PatItem::Any,
            PatItem::Any,
        ]);

        let pat = Pattern::parse("/vft /vft(rttiIType_GetERTTITypeString) /vft(pure) /vft(null) /vft(ret(0)) /vft(ret 0)").unwrap();
        assert_matches!(pat.parts(), &[
            PatItem::ByteCode(ByteCode::Vft(VftType::Any)),
            PatItem::ByteCode(ByteCode::Vft(VftType::Ref(_))),
            PatItem::ByteCode(ByteCode::Vft(VftType::PureCall)),
            PatItem::ByteCode(ByteCode::Vft(VftType::Null)),
            PatItem::ByteCode(ByteCode::Vft(VftType::Return(0))),
            PatItem::ByteCode(ByteCode::Vft(VftType::Return(0))),
        ]);
        if let Some(n) = pat.parts().get(1) {
            assert!(match n {
                PatItem::ByteCode(ByteCode::Vft(VftType::Ref(name))) => name == "rttiIType_GetERTTITypeString",
                _ => false
            });
        } else {
            assert!(false);
        }
    }

    #[test]
    fn return_correct_longest_seq() {
        let pat = Pattern::parse("8B ? 0D ? F9 5F 48 B8 ? BA 10").unwrap();
        assert_matches!(pat.longest_byte_sequence(), &[
            PatItem::Byte(0xF9),
            PatItem::Byte(0x5F),
            PatItem::Byte(0x48),
            PatItem::Byte(0xB8)
        ]);
    }

    #[test]
    fn match_valid_patterns() {
        let pat1 = Pattern::parse("FD 98 07 ? ? 49 C5").unwrap();
        let pat2 = Pattern::parse("? BB 5E 83 F1 ? 49").unwrap();
        let pat3 = Pattern::parse("BA (match:rel) 89 BF").unwrap();
        let haystack = [
            0x9C, 0x0D, 0x1C, 0x53, 0x1D, 0x35, 0xFD, 0x98, 0x07, 0x10, 0x22, 0x49, 0xC5, 0xBB, 0x5E, 0x83,
            0xF1, 0xBF, 0x49, 0x8E, 0x78, 0x32, 0x17, 0xC1, 0x6F, 0xBA, 0x83, 0x5B, 0x5D, 0x83, 0x89, 0xBF,
        ];
        let matches = multi_search([&pat1, &pat2, &pat3], &haystack, 0);
        assert_eq!(matches[0], [6]);
        assert_eq!(matches[1], [12]);
        assert_eq!(matches[2], [25]);
    }

    #[test]
    fn return_correct_groups() {
        let pat = Pattern::parse("BA CC (one:rel) FF 89 BF (two:rel) (three:rel) 56").unwrap();
        assert_matches!(pat.groups().collect::<Vec<_>>().as_slice(), &[
            ("one", VarType::Rel, 2),
            ("two", VarType::Rel, 9),
            ("three", VarType::Rel, 13)
        ]);
    }
}
