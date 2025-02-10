use std::path::PathBuf;

use bimap::BiHashMap;
use ecsl_index::PackageID;
use serde::Deserialize;

#[derive(Debug)]
pub struct EcslPackage {
    cr: PackageID,
    info: PackageInfo,
    dependencies: BiHashMap<String, PackageID>,
}

impl EcslPackage {
    pub fn new(cr: PackageID, info: PackageInfo) -> Self {
        Self {
            cr,
            info,
            dependencies: BiHashMap::new(),
        }
    }

    pub fn add_dependency(&mut self, s: String, cr: PackageID) {
        self.dependencies.insert(s, cr);
    }

    pub fn has_dependency(&self, cr: PackageID) -> bool {
        self.dependencies.contains_right(&cr)
    }

    pub fn dependencies(&mut self) -> impl Iterator<Item = (&String, &PackageID)> {
        self.dependencies.iter()
    }

    pub fn get_dependency<'a>(&self, dep: impl Into<&'a str>) -> Option<PackageID> {
        let s = dep.into().to_string();
        self.dependencies.get_by_left(&s).cloned()
    }

    pub fn id(&self) -> PackageID {
        self.cr
    }

    pub fn info(&self) -> &PackageInfo {
        &self.info
    }

    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn bundle_toml_path(&self) -> PathBuf {
        self.info.path.clone()
    }

    pub fn src_path(&self) -> PathBuf {
        let mut src = self.info.path.clone();
        src.push("src");
        src
    }

    pub fn file_path(&self, file_path: &PathBuf) -> PathBuf {
        let mut file = self.src_path();
        file.push(file_path);
        file.set_extension("ecsl");
        file
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageDependency {
    pub required_by: PackageID,
    pub id: PackageID,
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
