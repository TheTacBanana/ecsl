use std::collections::BTreeMap;
use std::io::{self, prelude::*, SeekFrom};
use std::marker::PhantomData;
use std::sync::RwLock;
use std::{fs::File, path::PathBuf};

use ecsl_bytecode::{Bytecode, FunctionBytecode, Immediate};
use ecsl_index::TyID;
use header::{FileType, SectionPointer, SectionType};
use log::debug;

pub mod header;

pub struct Assembler<T> {
    _phantom: PhantomData<T>,

    path: PathBuf,
    temp_path: PathBuf,
    file: File,

    file_type: FileType,

    const_data: Vec<u8>,

    functions: RwLock<BTreeMap<TyID, FunctionBytecode>>,

    sections: Vec<SectionPointer>,
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
            file_type: self.file_type,
            functions: self.functions,
            const_data: self.const_data,
            sections: self.sections,
        }
    }
}

impl Assembler<Pre> {
    pub fn new(name: String, root_path: &PathBuf) -> Self {
        let mut path = root_path.clone();
        path.push(format!("target/{}.ecsb", name));

        let mut temp_path = path.clone();
        temp_path.set_extension("ecsb.temp");

        let _ = std::fs::remove_file(&temp_path);
        std::fs::create_dir_all(temp_path.parent().unwrap()).unwrap();
        let file = File::create(&temp_path).unwrap();

        Self {
            _phantom: Default::default(),
            path,
            temp_path,
            file,
            file_type: FileType::Executable,
            const_data: Vec::new(),
            sections: Vec::new(),
            functions: Default::default(),
        }
    }

    /// Write header excluding entrypoint and section header
    pub fn write_temp_header(mut self) -> std::io::Result<Assembler<ConstData>> {
        // Write magic bytes
        self.file.write(&Self::MAGIC_BYTES)?;

        // Write Versions
        self.file.write(&Self::MAJOR_VERSION.to_be_bytes())?;
        self.file.write(&Self::MINOR_VERSION.to_be_bytes())?;

        // Write File Type
        self.file.write(&(self.file_type as u32).to_be_bytes())?;

        // Write entry point and section header as 0 to be written later
        self.file.write(&0u64.to_be_bytes())?;
        self.file.write(&0u64.to_be_bytes())?;

        Ok(self.cast())
    }
}

impl Assembler<ConstData> {
    /// Write the const data section
    pub fn write_const_data(self) -> std::io::Result<Assembler<Executable>> {
        debug!("TODO: Write Const Data");
        Ok(self.cast())
    }
}

impl Assembler<Executable> {
    pub fn include_function(&self, byt: FunctionBytecode) {
        let mut funcs = self.functions.write().unwrap();
        funcs.insert(byt.tyid, byt);
    }

    /// Write function bytecode
    pub fn write_bytecode(mut self, entry_point: TyID) -> std::io::Result<Assembler<Out>> {
        let start_pos = self.offset_to_alignment()?;

        let functions = self.functions.get_mut().unwrap();
        let mut function_offsets = BTreeMap::new();
        let mut total_offset = 0;
        for (fid, byt) in functions.iter() {
            function_offsets.insert(*fid, total_offset);
            total_offset = (total_offset + byt.total_size as u64).next_multiple_of(8);
        }

        // Rewrite Jumps
        for (fid, byt) in functions.iter_mut() {
            let func_offset = function_offsets.get(&fid).unwrap();
            for ins in byt.ins.iter_mut() {
                for op in ins.operand.iter_mut() {
                    match op {
                        Immediate::AddressOf(ty_id) => {
                            *op =
                                Immediate::ULong(start_pos + *function_offsets.get(&ty_id).unwrap())
                        }
                        Immediate::LabelOf(block_id) => {
                            *op = Immediate::ULong(
                                start_pos
                                    + *func_offset
                                    + *byt.block_offsets.get(block_id).unwrap() as u64,
                            );
                        }
                        _ => (),
                    }
                }
            }
        }

        // Func buffer
        let mut buffer = vec![0_u8; total_offset as usize];

        // TODO: Remove cloning
        for (fid, byt) in functions {
            let func_offset = function_offsets.get(fid).unwrap();
            debug!("{:?} at offset {}", fid, start_pos + func_offset);

            let mut temp_offset = 0;

            let mut bytecode_bin = Vec::new();
            for ins in byt.ins.iter() {
                debug!("{:?} {:?}", start_pos + func_offset + temp_offset, ins);
                let bytes = &ins.clone().to_bytecode().unwrap().to_bytes();
                temp_offset += bytes.len() as u64;
                bytecode_bin.extend_from_slice(bytes);
            }

            unsafe {
                std::ptr::copy_nonoverlapping(
                    bytecode_bin.as_ptr(),
                    (buffer[*func_offset as usize..]).as_mut_ptr(),
                    bytecode_bin.len(),
                );
            }
        }

        self.file.write(&buffer)?;
        let end_pos = self.file.seek(SeekFrom::End(0))?;

        self.sections.push(SectionPointer {
            section_type: SectionType::ExecutableCode,
            length: (end_pos as u64 - start_pos) as u32,
            address: start_pos,
        });

        let entry_point = start_pos + function_offsets.get(&entry_point).unwrap();

        self.file.seek(SeekFrom::Start(0x10))?;
        self.file.write(&entry_point.to_be_bytes())?;
        self.file.seek(SeekFrom::End(0))?;

        Ok(self.cast())
    }
}

impl Assembler<Out> {
    /// Write the section header at the end of the file and
    /// ammend the section header address
    /// Rename the file
    pub fn output(mut self) -> std::io::Result<PathBuf> {
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

        Ok(self.path)
    }
}
