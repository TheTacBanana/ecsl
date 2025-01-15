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

generate_index_type!(SymbolID);
generate_index_type!(SourceFileID);
generate_index_type!(CrateID);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    use super::CrateID;

    #[test]
    pub fn add() {
        assert_eq!(CrateID::new(1) + CrateID::new(2), CrateID::new(3))
    }
}
