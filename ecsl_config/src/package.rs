use std::{collections::{HashMap, VecDeque}, path::PathBuf};

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct BundleToml {
    pub package: PackageInfo,
    pub dependencies: HashMap<String, String>,
}

impl BundleToml {
    pub fn dependencies(&self) -> VecDeque<PackageDependency> {
        let mut dependencies = VecDeque::new();
        for (name, path) in self.dependencies.iter() {
            dependencies.push_back(PackageDependency {
                required_by: self.package.name.clone(),
                package_name: name.clone(),
                package_path: path.clone(),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageDependency {
    pub required_by: String,
    pub package_name: String,
    pub package_path: String,
}
