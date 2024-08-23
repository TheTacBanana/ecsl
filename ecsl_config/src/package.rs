use std::{collections::HashMap, path::PathBuf};

use ecsl_span::CrateID;
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct BundleToml {
    pub package: PackageInfo,
    pub dependencies: HashMap<String, String>,
}

impl BundleToml {
    pub fn get_dependencies(&self, from: CrateID) -> Vec<PackageDependency> {
        let mut dependencies = Vec::new();
        for (name, path) in self.dependencies.iter() {
            dependencies.push(PackageDependency {
                required_by: from,
                name: name.clone(),
                path: path.clone(),
            })
        }
        dependencies
    }
}

#[derive(Debug, Clone, Deserialize)]
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
}

impl PackageType {
    pub const fn file_name(&self) -> &'static str {
        match self {
            PackageType::Bin => "main.ecsl",
            PackageType::Lib => "lib.ecsl",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageDependency {
    pub required_by: CrateID,
    pub name: String,
    pub path: String,
}
