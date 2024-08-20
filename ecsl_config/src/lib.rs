use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fs::File,
    io::Read,
    path::PathBuf,
};

use ecsl_error::{EcslError, EcslResult, ErrorExt, ErrorLevel};
use package::{BundleToml, PackageDependency, PackageInfo};
use serde::Deserialize;

pub mod package;

#[derive(Debug)]
pub struct EcslRootConfig {
    /// Root Package
    pub root: PackageInfo,
    /// Transitive Dependencies
    pub deps: Vec<PackageInfo>,
    /// Dependencies which could not be resolved
    pub unresolved_deps: Vec<(PackageDependency, ConfigError)>,
}

impl EcslRootConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_root_config(path: &PathBuf) -> EcslResult<EcslRootConfig> {
        let root_bundle_toml = Self::load_package_info(path).ecsl_error(ErrorLevel::Error)?;

        // List of root dependencies
        let mut new_dependencies = root_bundle_toml.dependencies();

        // Seen package names
        let mut package_names = HashSet::<String>::new();
        package_names.insert(root_bundle_toml.package.name.clone());

        // Resolved and Failed to resolve dependencies
        let mut deps = Vec::new();
        let mut unresolved_deps = Vec::new();

        while let Some(dep) = new_dependencies.pop_front() {
            assert!(package_names.contains(&dep.required_by));

            if package_names.contains(&dep.package_name) {
                unresolved_deps.push((dep.clone(), ConfigError::CyclicDependency()))
            }

            let bundle_toml =
                Self::load_package_info(&dep.package_path.clone().into());

            match bundle_toml {
                Ok(bundle_toml) => {
                    new_dependencies.extend(bundle_toml.dependencies());

                    package_names.insert(bundle_toml.package.name.clone());
                    deps.push(bundle_toml.package);
                },
                Err(err) => {
                    unresolved_deps.push((dep, err))
                },
            }
        }

        Ok(EcslRootConfig {
            root: root_bundle_toml.package,
            deps,
            unresolved_deps,
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
    CyclicDependency(),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp: &str = match self {
            ConfigError::MissingBundleToml(p) => &format!("Could not open {:?}", p),
            ConfigError::FileReadError(e) => &format!("{e}"),
            ConfigError::MalformedFormat(e) => &format!("{e}"),
            ConfigError::CyclicDependency() => &format!("Cyclic Depencency"),
        };
        write!(f, "{}", temp)
    }
}

impl Error for ConfigError {}
