use std::{
    collections::HashMap,
    error::Error,
    fs::File,
    io::Read,
    path::PathBuf,
};

use ecsl_error::{EcslResult, ErrorExt, ErrorLevel};
use ecsl_span::CrateID;
use package::{BundleToml, PackageDependency, PackageInfo};
use petgraph::{algo, prelude::GraphMap, Directed};

pub mod package;

#[derive(Debug)]
pub struct EcslRootConfig {
    /// Packages
    pub packages: Vec<PackageInfo>,
    /// Dependencies which could not be resolved
    pub failed_packages: Vec<(PackageDependency, ConfigError)>,
    /// Crate which causes Cycle
    pub cycle: Option<CrateID>,
}

impl EcslRootConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_root_config(path: &PathBuf) -> EcslResult<EcslRootConfig> {
        let root_bundle_toml = Self::load_package_info(path).ecsl_error(ErrorLevel::Error)?;

        // Package Collection
        let mut packages = Vec::new();
        let mut failed_packages = Vec::new();
        let mut package_names = HashMap::<String, CrateID>::new();

        // Starting data
        packages.push(root_bundle_toml.package.clone());
        package_names.insert(root_bundle_toml.package.name.clone(), CrateID::new(0));

        // Graph used to detect dependency cycles
        let mut graph = GraphMap::<CrateID, (), Directed>::new();
        let root_index = graph.add_node(CrateID::new(0));

        let mut dependency_queue = root_bundle_toml.get_dependencies(root_index);

        // Traverse all dependencies building a graph
        while let Some(dep) = dependency_queue.pop() {
            let from_id = dep.required_by;
            let to_id = if package_names.contains_key(&dep.name) {
                *package_names.get(&dep.name).unwrap()
            } else {
                let new_id = CrateID::new(packages.len() as u32);

                // Load Bundle.toml file
                let bundle_toml = Self::load_package_info(&dep.path.clone().into());

                // Match result of file loading
                match bundle_toml {
                    // If successful create new node
                    Ok(bundle_toml) => {
                        // Load dependencies into queue
                        dependency_queue.extend(bundle_toml.get_dependencies(new_id));

                        package_names.insert(dep.name.clone(), new_id);
                        packages.push(bundle_toml.package);
                    }
                    // If failed consider it to be unresolved
                    Err(err) => {
                        failed_packages.push((dep, err));
                    }
                }

                new_id
            };

            graph.add_edge(from_id, to_id, ());
        }

        match algo::toposort(&graph, None) {
            // Topological sort failed because the dependencies are cyclic
            Err(cycle) => {
                let id = cycle.node_id();
                Ok(EcslRootConfig {
                    packages,
                    failed_packages,
                    cycle: Some(id),
                })
            }
            // If no cycles then sort the packages as per the toposort
            Ok(_) => {
                Ok(EcslRootConfig {
                    packages,
                    failed_packages,
                    cycle: None,
                })
            }
        }
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

    pub fn root(&self) -> &PackageInfo {
        &self.packages[0]
    }

    pub fn get_crate(&self, id: CrateID) -> Option<&PackageInfo> {
        self.packages.get(*id as usize)
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
