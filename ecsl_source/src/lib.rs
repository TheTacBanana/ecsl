use std::{collections::HashMap, fs::File, path::PathBuf};
use std::io::{Read, Result};

use lines::LineNumbers;
use pos::BytePos;

pub mod span;
pub mod pos;
pub mod lines;

#[derive(Debug, Clone)]
pub struct SourceMap {
    file_map: HashMap<SourceFileID, SourceFile>
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct SourceFileID(pub u32);

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub file_name: String,
    pub contents: String,
    pub lines: LineNumbers,
    //TODO: Crate ID
}

impl SourceFile {
    pub fn from_file(path: PathBuf) -> Result<Self> {
        let mut file = File::open(path)?;
        // let file_name = path.file_name().unwrap().into_string();

        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        // contents.is_ascii();

        let lines = LineNumbers::from(contents.as_str());




        Ok(SourceFile {
            file_name: todo!(),
            // file_name,
            contents,
            lines,
        })
    }
}
