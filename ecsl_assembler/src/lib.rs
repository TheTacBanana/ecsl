use std::io::{prelude::*, SeekFrom};
use std::{fs::File, path::PathBuf};

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
    pub const MAGIC_BYTES: &[u8] = &[0x45, 0x43, 0x53, 0x4C];
    pub const ALIGNMENT: u64 = 32;

    pub fn new() -> Self {
        Self {
            major_version: 1,
            minor_version: 0,
            file_type: FileType::Executable,
            const_data: Vec::new(),
            sections: Vec::new(),
        }
    }

    pub fn output(mut self, mut file_path: PathBuf) -> std::io::Result<()> {
        file_path.push("out.ecsb");

        let mut temp_path = file_path.clone();
        temp_path.set_extension("ecsb.temp");

        {
            let _ = std::fs::remove_file(&temp_path);
            let mut file = File::create_new(&temp_path)?;

            // Write header excluding entrypoint and section header
            self.write_temp_header(&mut file)?;

            //TODO: Write the const data section

            //TODO: Write the component info

            //TODO: Write the bytecode

            //TODO: Write the system info

            // Write the section header at the end of the file and
            // ammend the section header address
            self.write_section_header(&mut file)?;
        }

        std::fs::rename(&temp_path, &file_path)?;
        let _ = std::fs::remove_file(&temp_path);

        Ok(())
    }

    fn offset_to_alignment(&mut self, file: &mut File) -> std::io::Result<u64> {
        let start_of_header = file
            .seek(SeekFrom::Current(0))?
            .next_multiple_of(Self::ALIGNMENT);
        file.seek(SeekFrom::Start(start_of_header))?;
        Ok(start_of_header)
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

    fn write_section_header(&mut self, file: &mut File) -> std::io::Result<()> {
        let start_pos = self.offset_to_alignment(file)?;

        let len = self.sections.len() as u64;
        file.write(&len.to_be_bytes())?;

        for SectionPointer {
            section_type,
            length,
            address,
        } in &self.sections
        {
            file.write(&(*section_type as u32).to_be_bytes())?;
            file.write(&length.to_be_bytes())?;
            file.write(&address.to_be_bytes())?;
        }

        file.seek(SeekFrom::Start(0x18))?;
        file.write(&start_pos.to_be_bytes())?;

        Ok(())
    }
}
