use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
    path::PathBuf,
};

use ecsl_error::{EcslError, EcslResult, ErrorLevel};
use package::{BundleToml, PackageInfo};
use serde::{de::value::Error, Deserialize};

pub mod package;

#[derive(Debug)]
pub struct EcslRootConfig {
    /// Root Package
    pub root: PackageInfo,
    /// Transitive Dependencies
    pub deps: Vec<PackageInfo>,
    /// Dependencies which could not be resolved
    pub unresolved_deps: Vec<(PackageInfo, ConfigError)>,
}

impl EcslRootConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_root_config(path: &PathBuf) -> EcslResult<EcslRootConfig> {
        let root_bundle_toml = Self::load_package_info(path)
            .map_err(|e| EcslError::new(ErrorLevel::Error, e.to_string()))?;

        println!("{:?}", root_bundle_toml);

        let mut new_dependencies = root_bundle_toml.dependencies();

        let package_names = HashSet::<String>::new();
        let deps = Vec::new();
        let unresolved_deps = Vec::new();

        while let Some(dep) = new_dependencies.pop_front() {

        }

        Ok(EcslRootConfig {
            root: root_bundle_toml.package,
            deps: Vec::new(),
            unresolved_deps: Vec::new(),
        })
    }

    fn load_package_info(path: &PathBuf) -> Result<BundleToml, ConfigError> {
        let mut bundle_toml_path = path.clone();
        bundle_toml_path.push(Self::CONFIG_FILE);

        let mut file = File::open(&bundle_toml_path)
            .map_err(|_| ConfigError::MissingBundleToml(bundle_toml_path.clone()))?;

        let mut config_file = String::new();
        file.read_to_string(&mut config_file)
            .map_err(|e| ConfigError::FileReadError(e))?;

        let mut config: BundleToml =
            toml::from_str(&config_file).map_err(|e| ConfigError::MalformedFormat(e))?;
        config.package.path = path.clone();

        Ok(config)
    }
}

#[derive(Debug)]
pub enum ConfigError {
    MissingBundleToml(PathBuf),
    FileReadError(std::io::Error),
    MalformedFormat(toml::de::Error),
    MissingPackageDeclaration(PathBuf),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp: &str = match self {
            ConfigError::MissingBundleToml(p) => &format!("Could not open {:?}", p),
            ConfigError::MissingPackageDeclaration(p) => {
                &format!("Failed to read Package Declaration in {:?}", p)
            }
            ConfigError::FileReadError(e) => todo!(),
            ConfigError::MalformedFormat(e) => todo!(),
        };
        write!(f, "{}", temp)
    }
}

#[cfg(test)]
pub mod test {
    use std::path::PathBuf;

    use toml::Value;

    use crate::EcslRootConfig;

    #[test]
    fn load_config() {
        EcslRootConfig::new_root_config(&"../example/".into()).unwrap();
    }
}
