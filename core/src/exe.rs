use object::{Architecture, BinaryFormat, Endianness, Object, ObjectSection};

use crate::error::{Error, Result};

const TEXT_SECTION: &str = ".text";
const IDATA_SECTION: &str = ".rdata"; // idata in IDA is rdata i guess
const RDATA_SECTION: &str = ".rdata"; // this isn't actually offset like it is in IDA
const DATA_SECTION: &str = ".data";
const PDATA_SECTION: &str = ".pdata";

pub struct ExecutableData<'a> {
    text: &'a [u8],
    rdata: &'a [u8],
    data: &'a [u8],
    image_base: u64,
    text_offset: u64,
    idata_offset: u64,
    rdata_offset: u64,
    data_offset: u64,
    pdata_offset: u64,
}

impl<'a> ExecutableData<'a> {
    pub fn new(exe: &'a object::read::File<'a>) -> Result<Self> {
        for s in exe.sections() {
            log::info!("{}: 0x{:X}", s.name()?, s.address());
        }
        let text = exe
            .section_by_name(TEXT_SECTION)
            .ok_or(Error::MissingSection("text"))?;
        let idata = exe
            .section_by_name(IDATA_SECTION)
            .ok_or(Error::MissingSection("idata"))?;
        let rdata = exe
            .section_by_name(RDATA_SECTION)
            .ok_or(Error::MissingSection("rdata"))?;
        let data = exe
            .section_by_name(DATA_SECTION)
            .ok_or(Error::MissingSection("data"))?;
        let pdata = exe
            .section_by_name(PDATA_SECTION)
            .ok_or(Error::MissingSection("pdata"))?;

        // could also use pdata for function addr lookups

        let res = Self {
            text: text.data()?,
            rdata: rdata.data()?,
            data: data.data()?,
            image_base: exe.relative_address_base(),
            text_offset: text.address(),
            idata_offset: idata.address(),
            rdata_offset: rdata.address(),
            data_offset: data.address(),
            pdata_offset: pdata.address(),
        };
        Ok(res)
    }

    pub fn resolve_rel_text(&self, addr: u64) -> Result<u64> {
        let addr = addr as usize;
        let bytes = self
            .text
            .get(addr..addr + std::mem::size_of::<i32>())
            .ok_or(Error::InvalidAccess(addr))?
            .try_into()
            .unwrap();
        let rel = i32::from_ne_bytes(bytes);
        let abs = self.text_offset as i64 + addr as i64 + std::mem::size_of::<i32>() as i64 + rel as i64;
        Ok(abs as u64)
    }

    pub fn resolve_rel_rdata(&self, addr: u64) -> Result<u64> {
        let addr = addr as usize - self.rdata_offset as usize;
        let bytes = self
            .rdata
            .get(addr..addr + std::mem::size_of::<u64>())
            .ok_or(Error::InvalidAccess(addr))?
            .try_into()
            .unwrap();
        Ok(u64::from_ne_bytes(bytes))
    }

    pub fn resolve_call_rdata(&self, addr: u64) -> Result<u64> {
        let addr = addr as usize;
        let bytes = self
            .rdata
            .get(addr..addr + std::mem::size_of::<u64>())
            .ok_or(Error::InvalidAccess(addr))?
            .try_into()
            .map_err(|_| Error::CompileError("Try Into Error".to_owned()))?;
        let rel = u64::from_ne_bytes(bytes);
        let abs = self.image_base as i64 + rel as i64 - 0x140000000;
        Ok(abs as u64)
    }

    pub fn text(&'a self) -> &'a [u8] {
        self.text
    }

    pub fn rdata(&'a self) -> &'a [u8] {
        self.rdata
    }

    pub fn data(&'a self) -> &'a [u8] {
        self.data
    }

    pub fn text_offset(&'a self) -> u64 {
        self.text_offset
    }

    pub fn image_base(&'a self) -> u64 {
        self.image_base
    }

    pub fn text_offset_from_base(&'a self) -> u64 {
        self.text_offset - self.image_base
    }

    pub fn idata_offset_from_base(&'a self) -> u64 {
        self.idata_offset - self.image_base
    }

    pub fn rdata_offset_from_base(&'a self) -> u64 {
        self.rdata_offset - self.image_base
    }

    pub fn data_offset_from_base(&'a self) -> u64 {
        self.data_offset - self.image_base
    }

    pub fn pdata_offset_from_base(&'a self) -> u64 {
        self.pdata_offset - self.image_base
    }

    pub fn rel_offset(&'a self, rva: u64) -> u64 {
        let offset = if rva > self.data_offset_from_base() {
            rva - self.data_offset_from_base()
        } else if rva > self.rdata_offset_from_base() {
            rva - self.rdata_offset_from_base()
        } else if rva > self.idata_offset_from_base() {
            rva - self.idata_offset_from_base()
        } else if rva > self.text_offset_from_base() {
            rva - self.text_offset_from_base()
        } else {
            rva
        };
        offset
    }
}

#[derive(Debug)]
pub struct ExeProperties {
    architecture: Architecture,
    endianess: Endianness,
    image_base: u64,
}

impl ExeProperties {
    pub fn from_object<'a: 'b, 'b, O: Object<'a, 'b>>(obj: &'b O) -> Self {
        Self {
            architecture: obj.architecture(),
            endianess: obj.endianness(),
            image_base: obj.relative_address_base(),
        }
    }

    pub fn replicate_object<'a>(&self, format: BinaryFormat) -> object::write::Object<'a> {
        object::write::Object::new(format, self.architecture, self.endianess)
    }

    pub fn is64bit(&self) -> bool {
        match self.architecture {
            Architecture::X86_64 => true,
            Architecture::X86_64_X32 => false,
            _ => unimplemented!(),
        }
    }

    pub fn address_size(&self) -> u8 {
        match self.architecture {
            Architecture::X86_64 => 8,
            Architecture::X86_64_X32 => 4,
            _ => unimplemented!(),
        }
    }

    pub fn image_base(&self) -> u64 {
        self.image_base
    }
}
