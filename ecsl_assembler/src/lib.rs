use std::{fs::File, path::PathBuf};
use std::io::prelude::*;

use header::{FileType, SectionPointer};

pub mod header;

pub struct Assembler {
    pub major_version: u32,
    pub minor_version: u32,
    pub file_type: FileType,

    pub const_data: Vec<u8>,



    pub sections: Vec<SectionPointer>,
}

impl Assembler {
    pub const MAGIC_BYTES : &[u8] = &[0x45, 0x43, 0x53, 0x4C];

    pub fn output(mut self, mut file_path: PathBuf) -> std::io::Result<()> {
        // Create temp file to write to
        file_path.set_extension("ecsb.temp");
        let mut file = File::create_new(file_path)?;

        // Write header excluding entrypoint and section header
        self.write_temp_header(&mut file)?;

        // Write the const data section
        self.write_const_data(&mut file)?;

        // self.write_bytecode(&mut file);

        Ok(())
    }

    fn write_temp_header(&mut self, file: &mut File) -> std::io::Result<()> {
        // Write magic bytes
        file.write(&Self::MAGIC_BYTES)?;

        // Write Versions
        file.write(&self.major_version.to_be_bytes())?;
        file.write(&self.minor_version.to_be_bytes())?;

        // Write File Type
        file.write(&(self.file_type as u32).to_be_bytes())?;

        // Write entry point and section header as 0 to be written later
        file.write(&0u64.to_be_bytes())?;
        file.write(&0u64.to_be_bytes())?;

        Ok(())
    }

    fn write_const_data(&mut self, file: &mut File) -> std::io::Result<()> {
        if self.const_data.len() == 0 {
            return Ok(());
        }

        file.write(&self.const_data);



        if self.

        Ok(())
    }
}
