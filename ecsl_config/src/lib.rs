use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
    path::PathBuf,
};

use ecsl_error::{EcslError, EcslResult, ErrorTrait};
use serde::{de::value::Error, Deserialize};

#[derive(Debug, Clone)]
pub struct EcslRootConfig {
    /// Root Package
    pub root: PackageInfo,
    /// Transitive Dependencies
    pub deps: Vec<PackageInfo>,
    /// Dependencies which could not be resolved
    pub unresolved_deps: Vec<(PackageInfo, ConfigError)>,
}

#[derive(Debug, Clone)]
pub struct PackageInfo {
    pub path: PathBuf,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageType {
    Bin,
    Lib,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageDependency {
    pub required_by: String,
    pub package_path: String,
}

impl EcslRootConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_root_config(path: &PathBuf) -> EcslResult<EcslRootConfig> {
        Ok(EcslRootConfig {
            root: PackageInfo {
                path: path.clone(),
                name: "".to_owned()
            },
            deps: Vec::new(),
            unresolved_deps: Vec::new(),
        })

        // todo!()
        // let transitive_deps = Vec::new();
        // let (info, dependencies) = match EcslRootConfig::load_config(path) {
        //     Ok((info, dependencies)) => (info, dependencies),
        //     Err(e) => return Err(EcslError::new(e)),
        // };

        // let package_names = HashSet::<String>::new();

        // let deps = Vec::new();
        // let unresolved_deps = Vec::new();

        // for (name, path) in dependencies.iter() {
        //     if package_names.contains(name) {

        //     }

        //     match Self::load_config(path.into()) {
        //         Ok(_) => todo!(),
        //         Err(_) => todo!(),
        //     }

        // }

        // Ok(EcslRootConfig {
        //     root: info,
        //     deps,
        //     unresolved_deps,
        // })
    }

    fn load_config(mut path: PathBuf) -> Result<(PackageInfo, Vec<(String, String)>), ConfigError> {
        todo!();

        // path.push(EcslRootConfig::CONFIG_FILE);
        // let mut file =
        //     File::open(&path).map_err(|_| ConfigError::MissingBundleToml(path.clone()))?;

        // let mut config_file = String::new();
        // file.read_to_string(&mut config_file)
        //     .map_err(|_| ConfigError::FileReadError)?;

        // let config: EcslConfigDeserialize =
        //     toml::from_str(&config_file).map_err(|_| ConfigError::MalformedFormat)?;

        // let package = config.package.with_path(path);
        // let deps = config
        //     .dependencies
        //     .unwrap_or_default()
        //     .drain()
        //     .collect::<Vec<(String, String)>>();

        // Ok((package, deps))
    }
}

#[derive(Debug, Clone)]
pub enum ConfigError {
    MissingBundleToml(PathBuf),
    FileReadError,
    MalformedFormat,
    MissingPackageDeclaration(PathBuf),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp: &str = match self {
            ConfigError::MissingBundleToml(p) => &format!("Could not open {:?}", p),
            ConfigError::MissingPackageDeclaration(p) => {
                &format!("Failed to read Package Declaration in {:?}", p)
            }
            ConfigError::FileReadError => todo!(),
            ConfigError::MalformedFormat => todo!(),
        };
        write!(f, "{}", temp)
    }
}

impl ErrorTrait for ConfigError {}

// #[cfg(test)]
// pub mod test {
//     use std::path::PathBuf;

//     use toml::Value;

//     use crate::{EcslConfigDeserialize, EcslRootConfig};

//     #[test]
//     fn load_config() {
//         EcslRootConfig::new_root_config("./".into()).unwrap();
//     }
// }
