#[derive(Debug, Clone, Copy)]
pub enum FileType {
    Unknown = 0,
    Executable = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryPointKind {
    Unknown = 0,
    MainFn = 1,
    MainSysUnscheduled = 2,
    MainSysOnce = 3,
    MainSysLoop = 4,
}

#[derive(Debug, Clone, Copy)]
pub struct SectionPointer {
    pub section_type: SectionType,
    pub length: u32,
    pub address: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum SectionType {
    Unknown = 0,
    ComponentDefinitions = 1,
    SystemDependencies = 2,
    ExecutableCode = 3,
    Data = 4,
}
