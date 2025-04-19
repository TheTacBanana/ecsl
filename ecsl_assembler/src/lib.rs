use ecsl_bytecode::ext::BytecodeExt;
use ecsl_bytecode::function::FunctionBytecode;
use ecsl_bytecode::Immediate;
use ecsl_index::{AssemblerConstID, ComponentID, TyID};
use header::{EntryPointKind, FileType, SectionPointer, SectionType};
use log::debug;
use std::collections::{BTreeMap, HashMap};
use std::fs::OpenOptions;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::{prelude::*, SeekFrom};
use std::marker::PhantomData;
use std::os::unix::fs::FileExt;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::RwLock;
use std::{fs::File, path::PathBuf};

pub mod header;

#[derive(Debug)]
pub struct Assembler<T> {
    _phantom: PhantomData<T>,

    path: PathBuf,
    temp_path: PathBuf,
    file: File,
    cur_file_pos: AtomicU64,

    file_type: FileType,

    const_data_hash: RwLock<HashMap<u64, AssemblerConstID>>,
    const_data_offsets: RwLock<BTreeMap<AssemblerConstID, u64>>,
    const_data: RwLock<Vec<u8>>,

    schedule_fn_patch_marker: RwLock<Vec<(AssemblerConstID, u64)>>,

    functions: RwLock<BTreeMap<TyID, FunctionBytecode>>, // Reduce duplication

    sections: Vec<SectionPointer>,
}

pub struct Pre;
pub struct ConstData;
pub struct CompDefs;
pub struct Executable;
pub struct Schedule;
pub struct Out;

impl<T> Assembler<T> {
    pub const MAGIC_BYTES: &[u8] = &[0x45, 0x43, 0x53, 0x4C];
    pub const ALIGNMENT: u64 = 8;
    pub const MAJOR_VERSION: u32 = 1;
    pub const MINOR_VERSION: u32 = 0;

    fn current_file_pos(&self) -> u64 {
        self.cur_file_pos.load(Ordering::Relaxed)
    }

    fn offset_to_alignment(&mut self) -> std::io::Result<u64> {
        let start_of_header = self
            .file
            .seek(SeekFrom::Current(0))?
            .next_multiple_of(Self::ALIGNMENT);
        self.file.seek(SeekFrom::Start(start_of_header))?;
        self.cur_file_pos.store(start_of_header, Ordering::Relaxed);
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
            const_data_offsets: self.const_data_offsets,
            const_data_hash: self.const_data_hash,
            schedule_fn_patch_marker: self.schedule_fn_patch_marker,
            sections: self.sections,
            cur_file_pos: self.cur_file_pos,
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
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&temp_path)
            .unwrap();

        Self {
            _phantom: Default::default(),
            path,
            temp_path,
            file,
            file_type: FileType::Executable,
            const_data: Default::default(),
            const_data_offsets: Default::default(),
            sections: Vec::new(),
            functions: Default::default(),
            schedule_fn_patch_marker: Default::default(),
            const_data_hash: Default::default(),
            cur_file_pos: Default::default(),
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
        self.file.write(&(self.file_type as u16).to_be_bytes())?;

        // Write Entry Point Kind
        self.file.write(&(0 as u16).to_be_bytes())?;

        // Write entry point and section header as 0 to be written later
        self.file.write(&0u64.to_be_bytes())?;
        self.file.write(&0u64.to_be_bytes())?;

        self.offset_to_alignment()?;
        Ok(self.cast())
    }
}

impl Assembler<ConstData> {
    pub fn add_const_data(&self, bytes: Vec<u8>) -> AssemblerConstID {
        let mut hashes = self.const_data_hash.write().unwrap();
        let mut const_data = self.const_data.write().unwrap();
        let mut offsets = self.const_data_offsets.write().unwrap();

        let mut hasher = DefaultHasher::new();
        bytes.hash(&mut hasher);
        let hash_value = hasher.finish();

        if let Some(id) = hashes.get(&hash_value) {
            return *id;
        } else {
            let file_pos = self.current_file_pos() + const_data.len() as u64;
            const_data.extend_from_slice(&bytes);

            let next_id = AssemblerConstID::new(offsets.len());
            offsets.insert(next_id, file_pos);
            hashes.insert(hash_value, next_id);

            next_id
        }
    }

    pub fn get_offset(&self, id: AssemblerConstID) -> Option<u64> {
        let offsets = self.const_data_offsets.read().unwrap();
        offsets.get(&id).cloned()
    }

    pub fn add_fn_patch_marker(&self, id: AssemblerConstID, offset: u64) {
        let mut patches = self.schedule_fn_patch_marker.write().unwrap();
        patches.push((id, offset));
    }

    /// Write the const data section
    pub fn write_const_data(mut self) -> std::io::Result<Assembler<CompDefs>> {
        let start_pos = self.current_file_pos();

        {
            let const_data = self.const_data.read().unwrap();
            self.file.write(&const_data)?;
        }

        let end_pos = self.file.seek(SeekFrom::End(0))?;

        self.sections.push(SectionPointer {
            section_type: SectionType::Data,
            length: (end_pos as u64 - start_pos) as u32,
            address: start_pos,
        });

        Ok(self.cast())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ComponentDef {
    pub id: ComponentID,
    pub size: usize,
}

impl ComponentDef {
    pub fn into_bytes(self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&(self.id.inner() as u32).to_be_bytes());
        bytes.extend_from_slice(&(self.size as u32).to_be_bytes());
        bytes
    }
}

impl Assembler<CompDefs> {
    /// Write the const data section
    pub fn write_comp_defs(
        mut self,
        mut defs: Vec<ComponentDef>,
    ) -> std::io::Result<Assembler<Executable>> {
        let start_pos = self.offset_to_alignment()?;

        let mut buffer = Vec::new();

        let defs = defs
            .drain(..)
            .filter(|def| def.id != ComponentID::ZERO)
            .collect::<Vec<_>>();

        // Write length of component defs
        buffer.extend_from_slice(&(defs.len() as u32).to_be_bytes());

        // Write each def
        for def in defs {
            buffer.extend_from_slice(&def.into_bytes());
        }

        self.file.write(&buffer)?;
        let end_pos = self.file.seek(SeekFrom::End(0))?;

        self.sections.push(SectionPointer {
            section_type: SectionType::ComponentDefinitions,
            length: (end_pos as u64 - start_pos) as u32,
            address: start_pos,
        });

        Ok(self.cast())
    }
}

impl Assembler<Executable> {
    pub fn include_function(&self, byt: FunctionBytecode) {
        let mut funcs = self.functions.write().unwrap();
        funcs.insert(byt.tyid, byt);
    }

    /// Write function bytecode
    pub fn write_bytecode(
        mut self,
        entry_point: (TyID, EntryPointKind),
    ) -> std::io::Result<Assembler<Out>> {
        let start_pos = self.offset_to_alignment()?;

        let functions = self.functions.get_mut().unwrap();
        let functions = std::mem::take(functions);

        let mut function_offsets = BTreeMap::new();
        let mut total_offset = 0;
        for (fid, byt) in functions.iter() {
            function_offsets.insert(*fid, total_offset);
            total_offset = (total_offset + byt.bytecode_size() as u64).next_multiple_of(8);
        }

        let mut bytecode_offsets = BTreeMap::new();
        for (_, byt) in functions.into_iter() {
            bytecode_offsets.insert(byt.tyid, byt.into_instructions());
        }

        // Rewrite Jumps
        for (fid, (bytecode, block_offsets)) in bytecode_offsets.iter_mut() {
            let func_offset = function_offsets.get(&fid).unwrap();

            for ins in bytecode.iter_mut() {
                for op in ins.operand.iter_mut() {
                    match op {
                        Immediate::AddressOf(ty_id) => {
                            *op = Immediate::ULong(
                                start_pos
                                    + *function_offsets.get(&ty_id).expect(&format!(
                                        "Internal Compiler Error: Function {:?} not found",
                                        entry_point
                                    )),
                            )
                        }
                        Immediate::LabelOf(block_id) => {
                            *op = Immediate::ULong(
                                start_pos
                                    + *func_offset
                                    + *block_offsets.get(block_id).unwrap() as u64,
                            );
                        }
                        Immediate::ConstAddressOf(const_id) => {
                            *op = Immediate::ULong(
                                *self
                                    .const_data_offsets
                                    .read()
                                    .unwrap()
                                    .get(const_id)
                                    .unwrap(),
                            )
                        }
                        _ => (),
                    }
                }
            }
        }

        // Func buffer
        let mut buffer = vec![0_u8; total_offset as usize];

        // TODO: Remove cloning
        for (fid, (bytecode, _)) in bytecode_offsets {
            let func_offset = function_offsets.get(&fid).unwrap();
            debug!("{:?} at offset {}", fid, start_pos + func_offset);

            let mut temp_offset = 0;

            let mut bytecode_bin = Vec::new();

            for ins in bytecode.iter() {
                debug!("{:?} {}", start_pos + func_offset + temp_offset, ins);
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

        let entry_point_addr = start_pos
            + function_offsets.get(&entry_point.0).expect(&format!(
                "Internal Compiler Error: Entry point {:?} not found",
                entry_point
            ));

        // Patch Entry Point Kind
        self.file.seek(SeekFrom::Start(14))?;
        self.file.write(&(entry_point.1 as u16).to_be_bytes())?;

        // Patch Entry point addr
        self.file.seek(SeekFrom::Start(16))?;
        self.file.write_at(&entry_point_addr.to_be_bytes(), 16)?;

        // Patch
        {
            let markers = self.schedule_fn_patch_marker.read().unwrap();
            let offsets = self.const_data_offsets.read().unwrap();

            for (id, offset) in markers.iter() {
                let offset = *offsets.get(id).unwrap() + offset;

                let mut bytes = [0u8; 8];
                self.file.read_at(&mut bytes, offset).unwrap();
                let bytes = function_offsets
                    .get(&TyID::new(u64::from_be_bytes(bytes) as usize))
                    .inspect(|v| debug!("{:?}", v))
                    .unwrap()
                    .to_be_bytes();

                self.file.write_at(&bytes, offset).unwrap();
            }
        }

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

        let len = self.sections.len() as u32;
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

        self.file.write_at(&start_pos.to_be_bytes(), 0x18)?;

        std::fs::rename(&self.temp_path, &self.path)?;
        let _ = std::fs::remove_file(&self.temp_path);

        Ok(self.path)
    }
}
