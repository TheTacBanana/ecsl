use crate::{
    bundle_toml::BundleToml,
    package::{EcslPackage, PackageDependency},
};
use bimap::BiHashMap;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, EcslResult, ErrorLevel};
use ecsl_index::CrateID;
use log::info;
use petgraph::{algo, prelude::GraphMap, Directed};
use std::{collections::BTreeMap, path::PathBuf};

#[derive(Debug)]
pub struct EcslConfig {
    pub abs_paths: BiHashMap<PathBuf, CrateID>,
    pub packages: BTreeMap<CrateID, EcslPackage>,

    pub root_id: CrateID,
    pub std_path: PathBuf,
    pub std_id: CrateID,

    dependency_queue: Vec<PackageDependency>,

    pub cycle: Option<CrateID>,
}

impl EcslConfig {
    pub fn new_config(
        path: &PathBuf,
        std_path: &PathBuf,
        diag: DiagConn,
    ) -> EcslResult<EcslConfig> {
        let mut config = EcslConfig {
            abs_paths: BiHashMap::new(),
            packages: BTreeMap::new(),
            std_id: CrateID::ZERO,
            root_id: CrateID::ZERO,
            std_path: std_path.clone(),
            dependency_queue: Vec::new(),
            cycle: None,
        };

        {
            // Load project Bundle.toml first to check it exists
            let root_bundle_toml = Self::load_bundle_toml(path)?;

            // Load std lib second but process it first
            config.std_id = match Self::load_bundle_toml(std_path) {
                Ok(bundle_toml) => config.include_bundle_toml(bundle_toml, diag.clone()),
                Err(err) => {
                    return Err(err.with_note(|_| "Failed to load standard library".to_string()));
                }
            };
            // Can now set the canon path as it exists
            config.std_path = config.std_path.canonicalize().unwrap();

            // Finally include the root
            config.root_id = config.include_bundle_toml(root_bundle_toml, diag.clone());
        }

        // Graph used to detect dependency cycles
        let mut graph = GraphMap::<CrateID, (), Directed>::new();
        graph.add_node(config.root_id);
        graph.add_node(config.std_id);
        graph.add_edge(config.root_id, config.std_id, ());

        // Traverse all dependencies building a graph
        while let Some(dep) = config.dependency_queue.pop() {
            let from_id = dep.required_by;
            let to_id = if config.packages.contains_key(&dep.id) {
                dep.id
            } else {
                info!("Loading dependency {}", dep.name);
                // Load Bundle.toml file
                let path = config.abs_paths.get_by_right(&dep.id).unwrap();
                match Self::load_bundle_toml(path) {
                    Ok(bundle_toml) => config.include_bundle_toml(bundle_toml, diag.clone()),
                    Err(err) => {
                        diag.push_error(err);
                        continue;
                    }
                }
            };

            graph.add_edge(from_id, to_id, ());
        }

        // Topological sort failed at least one dependency is cyclic
        if let Err(cycle) = algo::toposort(&graph, None) {
            config.cycle = Some(cycle.node_id());
        }

        Ok(config)
    }

    fn load_bundle_toml(path: &PathBuf) -> EcslResult<BundleToml> {
        info!("Loading Bundle.toml from path {:?}", path);

        let mut bundle_toml_path = path.clone();
        bundle_toml_path.push("Bundle.toml");

        BundleToml::deserialize(&bundle_toml_path)
            .map_err(|e| EcslError::new(ErrorLevel::Error, e.to_string()))
            .with_path(|_| path.clone())
    }

    fn include_bundle_toml(&mut self, mut bundle_toml: BundleToml, diag: DiagConn) -> CrateID {
        let package_id = self.crate_id_from_path(&bundle_toml.package.path);
        let mut package = EcslPackage::new(package_id, bundle_toml.package.clone());

        info!(
            "Including package '{}' with id {}",
            bundle_toml.package.name, package_id
        );

        // Convert out of a hashmap
        let mut dependencies = bundle_toml
            .dependencies
            .drain()
            .collect::<Vec<(String, PathBuf)>>();

        // Add std as a dependency for all crates
        if package_id != self.std_id {
            dependencies.push(("std".to_string(), self.std_path.clone()));
        }

        for (name, path) in dependencies {
            let dependency_path = if path.is_relative() {
                let mut p = bundle_toml.package.path.clone();
                p.push(path.clone());
                p
            } else {
                path.clone()
            };

            match dependency_path.canonicalize() {
                Ok(path) => {
                    let dep_id = self.crate_id_from_path(&path);
                    if package.has_dependency(dep_id) {
                        diag.push_error(
                            EcslError::new(
                                ErrorLevel::Error,
                                &format!("Dependency {:?} is used multiple times", path),
                            )
                            .with_path(|_| bundle_toml.package.path.clone()),
                        );
                        continue;
                    }

                    info!(
                        "Added dependency '{}' with id {} to '{}'",
                        name,
                        dep_id,
                        package.name()
                    );
                    package.add_dependency(name.clone(), dep_id);

                    self.dependency_queue.push(PackageDependency {
                        required_by: package_id,
                        id: dep_id,
                        name,
                        path,
                    });
                }
                Err(_) => {
                    diag.push_error(EcslError::new(
                        ErrorLevel::Error,
                        &format!("Cannot locate dependency '{}' at path {:?}", name, path),
                    ));
                }
            }
        }
        self.packages.insert(package_id, package);
        package_id
    }

    /// Get CrateID from canonicalized path
    pub fn crate_id_from_path(&mut self, path: &PathBuf) -> CrateID {
        let path = path.canonicalize().ok().unwrap();

        let next_id = CrateID::new(self.abs_paths.len());
        if self.abs_paths.contains_left(&path) {
            *self.abs_paths.get_by_left(&path).unwrap()
        } else {
            self.abs_paths.insert(path.clone(), next_id);
            next_id
        }
    }

    pub fn get_crate(&self, id: CrateID) -> Option<&EcslPackage> {
        self.packages.get(&id)
    }
}
