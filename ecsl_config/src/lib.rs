use std::{collections::HashMap, path::PathBuf};

use ecsl_error::{ext::EcslErrorExt, EcslError, EcslResult, ErrorLevel};
use ecsl_span::index::CrateID;
use package::{BundleToml, PackageDependency, PackageInfo};
use petgraph::{algo, prelude::GraphMap, Directed};

pub mod package;

#[derive(Debug)]
pub struct EcslRootConfig {
    /// Packages
    pub packages: Vec<PackageInfo>,
    /// Dependencies which could not be resolved
    pub failed_packages: Vec<(PackageDependency, EcslError)>,
    /// Crate which causes Cycle
    pub cycle: Option<CrateID>,
}

impl EcslRootConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_root_config(path: &PathBuf) -> EcslResult<EcslRootConfig> {
        let root_bundle_toml = Self::load_package_info(path)?;

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
                let new_id = CrateID::new(packages.len());

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
            Ok(_) => Ok(EcslRootConfig {
                packages,
                failed_packages,
                cycle: None,
            }),
        }
    }

    fn load_package_info(path: &PathBuf) -> EcslResult<BundleToml> {
        let mut bundle_toml_path = path.clone();
        bundle_toml_path.push(Self::CONFIG_FILE);

        BundleToml::deserialize(&bundle_toml_path)
            .map_err(|e| EcslError::new(ErrorLevel::Error, e.to_string()))
            .with_path(|_| bundle_toml_path.clone())
    }

    pub fn root(&self) -> &PackageInfo {
        &self.packages[0]
    }

    pub fn get_crate(&self, id: CrateID) -> Option<&PackageInfo> {
        self.packages.get(id.inner())
    }
}
