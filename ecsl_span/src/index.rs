use std::ops::{Add, Sub};

macro_rules! generate_index_type {
    ($struct_name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
        pub struct $struct_name(pub u32);

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

        impl Add<$struct_name> for $struct_name {
            type Output = $struct_name;

            fn add(self, rhs: $struct_name) -> Self::Output {
                $struct_name(self.0 + rhs.0)
            }
        }

        impl Sub<$struct_name> for $struct_name {
            type Output = $struct_name;

            fn sub(self, rhs: $struct_name) -> Self::Output {
                $struct_name(self.0 + rhs.0)
            }
        }
    };
}

generate_index_type!(CrateID);
generate_index_type!(SourceFileID);
generate_index_type!(LineNumber);
generate_index_type!(BytePos);