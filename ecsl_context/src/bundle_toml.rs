use std::{collections::HashMap, error::Error, fs::File, io::Read, path::PathBuf};

use serde::Deserialize;

use crate::package::PackageInfo;

#[derive(Debug, Clone, Deserialize)]
pub struct BundleToml {
    pub package: PackageInfo,
    pub dependencies: HashMap<String, PathBuf>,
}

impl BundleToml {
    pub fn deserialize(path: &PathBuf) -> Result<BundleToml, BundleTomlError> {
        let mut file =
            File::open(&path).map_err(|_| BundleTomlError::MissingBundleToml(path.clone()))?;

        let mut config_file = String::new();
        file.read_to_string(&mut config_file)
            .map_err(|e| BundleTomlError::FileReadError(e))?;

        let mut config: BundleToml =
            toml::from_str(&config_file).map_err(|e| BundleTomlError::MalformedFormat(e))?;

        let mut root_path = path.clone().canonicalize().unwrap();
        root_path.pop();
        config.package.path = root_path;

        Ok(config)
    }
}

#[derive(Debug)]
pub enum BundleTomlError {
    MissingBundleToml(PathBuf),
    FileReadError(std::io::Error),
    MalformedFormat(toml::de::Error),
}

impl std::fmt::Display for BundleTomlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp: &str = match self {
            BundleTomlError::MissingBundleToml(p) => &format!("Could not open {:?}", p),
            BundleTomlError::FileReadError(e) => &format!("{e}"),
            BundleTomlError::MalformedFormat(e) => &format!("{}", e.message()),
        };
        write!(f, "{}", temp)
    }
}

impl Error for BundleTomlError {}
