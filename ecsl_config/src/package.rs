use std::{collections::HashMap, error::Error, fs::File, io::Read, path::PathBuf};

use ecsl_span::index::CrateID;
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct BundleToml {
    pub package: PackageInfo,
    pub dependencies: HashMap<String, String>,
}

impl BundleToml {
    pub fn deserialize(path: &PathBuf) -> Result<BundleToml, ConfigError> {
        let mut file = File::open(&path)
            .map_err(|_| ConfigError::MissingBundleToml(path.clone()))?;

        let mut config_file = String::new();
        file.read_to_string(&mut config_file)
            .map_err(|e| ConfigError::FileReadError(e))?;

        let mut config: BundleToml =
            toml::from_str(&config_file).map_err(|e| ConfigError::MalformedFormat(e))?;

        let mut root_path = path.clone();
        root_path.pop();
        config.package.path = root_path;

        Ok(config)
    }

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

#[derive(Debug)]
pub enum ConfigError {
    MissingBundleToml(PathBuf),
    FileReadError(std::io::Error),
    MalformedFormat(toml::de::Error),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp: &str = match self {
            ConfigError::MissingBundleToml(p) => &format!("Could not open {:?}", p),
            ConfigError::FileReadError(e) => &format!("{e}"),
            ConfigError::MalformedFormat(e) => &format!("{}", e.message()),
        };
        write!(f, "{}", temp)
    }
}

impl Error for ConfigError {}
