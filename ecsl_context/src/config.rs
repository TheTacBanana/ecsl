use crate::{
    bundle_toml::BundleToml,
    package::{EcslPackage, PackageDependency},
};
use bimap::BiHashMap;
use ecsl_diagnostics::Diagnostics;
use ecsl_error::{EcslError, EcslResult, ErrorLevel};
use ecsl_index::CrateID;
use petgraph::{algo, prelude::GraphMap, Directed};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
    sync::Arc,
};

#[derive(Debug)]
pub struct EcslConfig {
    pub abs_paths: BiHashMap<PathBuf, CrateID>,
    pub packages: BTreeMap<CrateID, EcslPackage>,
    pub failed_packages: BTreeSet<CrateID>,

    dependency_queue: Vec<PackageDependency>,

    pub cycle: Option<CrateID>,
}

impl EcslConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_config(path: &PathBuf, diag: Arc<Diagnostics>) -> EcslResult<EcslConfig> {
        let root_bundle_toml = Self::load_bundle_toml(path)?;

        let mut config = EcslConfig {
            abs_paths: BiHashMap::new(),
            packages: BTreeMap::new(),
            failed_packages: BTreeSet::new(),
            dependency_queue: Vec::new(),
            cycle: None,
        };
        config.include_bundle_toml(root_bundle_toml);

        // Graph used to detect dependency cycles
        let mut graph = GraphMap::<CrateID, (), Directed>::new();
        graph.add_node(CrateID::new(0));

        // Traverse all dependencies building a graph
        while let Some(dep) = config.dependency_queue.pop() {
            let from_id = dep.required_by;
            let to_id = if config.packages.contains_key(&dep.id) {
                dep.id
            } else {
                // Load Bundle.toml file
                let path = config.abs_paths.get_by_right(&dep.id).unwrap();
                match Self::load_bundle_toml(path) {
                    Ok(bundle_toml) => config.include_bundle_toml(bundle_toml),
                    Err(err) => {
                        diag.push_error(err);
                        config.failed_packages.insert(dep.id);
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
        let mut bundle_toml_path = path.clone();
        bundle_toml_path.push(Self::CONFIG_FILE);

        BundleToml::deserialize(&bundle_toml_path)
            .map_err(|e| EcslError::new(ErrorLevel::Error, e.to_string()))
    }

    fn include_bundle_toml(&mut self, bundle_toml: BundleToml) -> CrateID {
        let package_id = self.crate_id_from_path(&bundle_toml.package.path);
        let mut package = EcslPackage {
            id: package_id,
            info: bundle_toml.package.clone(),
            dependencies: BTreeMap::new(),
        };

        for (name, path) in bundle_toml.dependencies {
            let dependency_path = if path.is_relative() {
                let mut p = bundle_toml.package.path.clone();
                p.push(path.clone());
                std::path::absolute(p).unwrap()
            } else {
                path.clone()
            };

            let dep_id = self.crate_id_from_path(&dependency_path);
            package.dependencies.insert(name.clone(), dep_id);

            self.dependency_queue.push(PackageDependency {
                required_by: package_id,
                id: dep_id,
                name,
                path,
            });
        }

        self.packages.insert(package_id, package);
        package_id
    }

    /// Get CrateID from absolute path
    pub fn crate_id_from_path(&mut self, path: &PathBuf) -> CrateID {
        let next_id = CrateID::new(self.abs_paths.len());
        if self.abs_paths.contains_left(path) {
            *self.abs_paths.get_by_left(path).unwrap()
        } else {
            self.abs_paths.insert(path.clone(), next_id);
            next_id
        }
    }

    pub fn get_crate(&self, id: CrateID) -> Option<&EcslPackage> {
        self.packages.get(&id)
    }
}
