use ecsl_bytecode::Address;


#[derive(Debug, Clone, Copy)]
pub enum FileType {
    Unknown = 0,
    Executable = 1,
}

pub struct SectionPointer {
    pub section_type: SectionType,
    pub length: u32,
    pub address: Address,
}

#[derive(Debug, Clone, Copy)]
pub enum SectionType {
    Unknown = 0,
    ComponentDefinitions = 1,
    SystemDependencies = 2,
    ExecutableCode = 3,
    Data = 4,
}
