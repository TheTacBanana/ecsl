use std::{collections::BTreeMap, path::PathBuf};

use ecsl_index::CrateID;
use serde::Deserialize;

#[derive(Debug)]
pub struct EcslPackage {
    pub id: CrateID,
    pub info: PackageInfo,
    // pub ty: PackageType,
    pub dependencies: BTreeMap<String, CrateID>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageDependency {
    pub required_by: CrateID,
    pub id: CrateID,
    pub name: String,
    pub path: PathBuf,
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct PackageInfo {
    #[serde(skip)]
    pub path: PathBuf,
    pub name: String,
    pub version: String,
}

impl std::fmt::Display for PackageInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.path.to_str().unwrap(),
            self.name,
            self.version
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageType {
    Bin,
    Lib,
    Unknown,
}
