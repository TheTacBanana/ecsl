use std::collections::BTreeMap;
use std::io::{prelude::*, SeekFrom};
use std::marker::PhantomData;
use std::{fs::File, path::PathBuf};

use ecsl_bytecode::{Bytecode, FunctionBytecode};
use ecsl_index::TyID;
use header::{FileType, SectionPointer, SectionType};
use log::debug;

pub mod header;

pub struct Assembler<T> {
    _phantom: PhantomData<T>,

    pub path: PathBuf,
    pub temp_path: PathBuf,
    pub file: File,

    pub major_version: u32,
    pub minor_version: u32,
    pub file_type: FileType,

    pub functions: BTreeMap<TyID, FunctionBytecode>,
    pub function_offsets: BTreeMap<TyID, usize>,

    pub const_data: Vec<u8>,

    pub sections: Vec<SectionPointer>,
}

pub struct Pre;
pub struct ConstData;
pub struct Executable;
pub struct Out;

impl<T> Assembler<T> {
    pub const MAGIC_BYTES: &[u8] = &[0x45, 0x43, 0x53, 0x4C];
    pub const ALIGNMENT: u64 = 32;
    pub const MAJOR_VERSION: u32 = 1;
    pub const MINOR_VERSION: u32 = 0;

    fn offset_to_alignment(&mut self) -> std::io::Result<u64> {
        let start_of_header = self
            .file
            .seek(SeekFrom::Current(0))?
            .next_multiple_of(Self::ALIGNMENT);
        self.file.seek(SeekFrom::Start(start_of_header))?;
        Ok(start_of_header)
    }

    fn cast<U>(self) -> Assembler<U> {
        Assembler {
            _phantom: Default::default(),
            path: self.path,
            temp_path: self.temp_path,
            file: self.file,
            major_version: self.major_version,
            minor_version: self.minor_version,
            file_type: self.file_type,
            functions: self.functions,
            function_offsets: self.function_offsets,
            const_data: self.const_data,
            sections: self.sections,
        }
    }
}

impl Assembler<Pre> {
    pub fn new(name: String, root_path: &PathBuf) -> Self {
        let mut path = root_path.clone();
        path.push(format!("target/{:?}.ecsb", name));

        let mut temp_path = path.clone();
        temp_path.set_extension("ecsb.temp");

        let _ = std::fs::remove_file(&temp_path);
        let file = File::create_new(&temp_path).unwrap();

        Self {
            _phantom: Default::default(),
            major_version: Self::MAJOR_VERSION,
            minor_version: Self::MINOR_VERSION,
            path,
            temp_path,
            file,
            file_type: FileType::Executable,
            const_data: Vec::new(),
            sections: Vec::new(),
            functions: Default::default(),
            function_offsets: Default::default(),
        }
    }

    /// Write header excluding entrypoint and section header
    pub fn write_temp_header(mut self) -> Assembler<ConstData> {
        // Write magic bytes
        self.file.write(&Self::MAGIC_BYTES).unwrap();

        // Write Versions
        self.file.write(&self.major_version.to_be_bytes()).unwrap();
        self.file.write(&self.minor_version.to_be_bytes()).unwrap();

        // Write File Type
        self.file
            .write(&(self.file_type as u32).to_be_bytes())
            .unwrap();

        // Write entry point and section header as 0 to be written later
        self.file.write(&0u64.to_be_bytes()).unwrap();
        self.file.write(&0u64.to_be_bytes()).unwrap();

        self.cast()
    }
}

impl Assembler<ConstData> {
    /// Write the const data section
    pub fn write_const_data(self) -> Assembler<Executable> {
        debug!("TODO: Write Const Data");
        self.cast()
    }
}

impl Assembler<Executable> {
    pub fn include_function(&mut self, byt: FunctionBytecode) {
        self.functions.insert(byt.tyid, byt);
    }

    /// Write function bytecode
    pub fn write_bytecode(mut self) -> std::io::Result<Assembler<Out>> {
        let start_pos = self.offset_to_alignment()?;

        let inst = vec![
            Bytecode::PSHI(3),
            Bytecode::PSHI(2),
            Bytecode::ADDI,
            Bytecode::HALT,
        ];

        let mut bytes = Vec::new();

        for i in inst {
            bytes.extend(i.to_bytes());
        }

        self.file.write(&bytes)?;
        let end_pos = self.file.seek(SeekFrom::End(0))?;

        self.sections.push(SectionPointer {
            section_type: SectionType::ExecutableCode,
            length: (end_pos as u64 - start_pos) as u32,
            address: start_pos,
        });

        self.file.seek(SeekFrom::Start(0x10))?;
        self.file.write(&start_pos.to_be_bytes())?;
        self.file.seek(SeekFrom::End(0))?;

        Ok(self.cast())
    }
}

impl Assembler<Out> {
    /// Write the section header at the end of the file and
    /// ammend the section header address
    /// Rename the file
    pub fn output(mut self) -> std::io::Result<()> {
        let start_pos = self.offset_to_alignment()?;

        let len = self.sections.len() as u64;
        self.file.write(&len.to_be_bytes())?;

        for SectionPointer {
            section_type,
            length,
            address,
        } in &self.sections
        {
            self.file.write(&(*section_type as u32).to_be_bytes())?;
            self.file.write(&length.to_be_bytes())?;
            self.file.write(&address.to_be_bytes())?;
        }

        self.file.seek(SeekFrom::Start(0x18))?;
        self.file.write(&start_pos.to_be_bytes())?;

        std::fs::rename(&self.temp_path, &self.path)?;
        let _ = std::fs::remove_file(&self.temp_path);

        Ok(())
    }
}
