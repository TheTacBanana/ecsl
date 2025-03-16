#[macro_export]
macro_rules! generate_index_type {
    ($struct_name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
        pub struct $struct_name(u32);

        impl $struct_name {
            pub const ZERO: $struct_name = $struct_name(0);
            pub const ONE: $struct_name = $struct_name(1);

            pub fn new(id: usize) -> Self {
                $struct_name(id as u32)
            }

            pub fn inner(&self) -> usize {
                self.0 as usize
            }
        }

        impl std::fmt::Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl std::ops::Add<$struct_name> for $struct_name {
            type Output = $struct_name;

            fn add(self, rhs: $struct_name) -> Self::Output {
                $struct_name(self.0 + rhs.0)
            }
        }

        impl std::ops::Sub<$struct_name> for $struct_name {
            type Output = $struct_name;

            fn sub(self, rhs: $struct_name) -> Self::Output {
                $struct_name(self.0 - rhs.0)
            }
        }
    };
}

// General Indexes
generate_index_type!(SourceFileID);
generate_index_type!(PackageID);

// AST Indexes
generate_index_type!(SymbolID);

// TIR Indexes
generate_index_type!(TyID);
impl TyID {
    pub const UNKNOWN: TyID = TyID(0);
    pub const BOTTOM: TyID = TyID(1);
}

generate_index_type!(VariantID);
generate_index_type!(FieldID);

// GIR indexes
generate_index_type!(BlockID);
generate_index_type!(LocalID);
generate_index_type!(ConstID);

// Assembly indexs
generate_index_type!(AssemblerConstID);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalID(SymbolID, SourceFileID);

impl GlobalID {
    pub fn new(s: SymbolID, f: SourceFileID) -> Self {
        GlobalID(s, f)
    }

    pub fn symbol(&self) -> SymbolID {
        self.0
    }

    pub fn source_file(&self) -> SourceFileID {
        self.1
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineNumberColumn {
    ln: usize,
    col: usize,
}

impl LineNumberColumn {
    pub fn new(ln: usize, col: usize) -> Self {
        Self { ln, col }
    }

    pub fn ln(&self) -> usize {
        self.ln
    }

    pub fn col(&self) -> usize {
        self.col
    }
}

impl std::fmt::Display for LineNumberColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:{}", self.ln, self.col)
    }
}

#[cfg(test)]
pub mod test {
    use super::PackageID;

    #[test]
    pub fn add() {
        assert_eq!(PackageID::new(1) + PackageID::new(2), PackageID::new(3))
    }
}
