use crate::{
    bundle_toml::BundleToml,
    package::{EcslPackage, PackageDependency},
};
use bimap::BiHashMap;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{EcslError, EcslResult, ErrorLevel};
use ecsl_index::CrateID;
use petgraph::{algo, prelude::GraphMap, Directed};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
};

#[derive(Debug)]
pub struct EcslConfig {
    pub abs_paths: BiHashMap<PathBuf, CrateID>,
    pub packages: BTreeMap<CrateID, EcslPackage>,

    dependency_queue: Vec<PackageDependency>,

    pub cycle: Option<CrateID>,
}

impl EcslConfig {
    pub const CONFIG_FILE: &'static str = "Bundle.toml";

    pub fn new_config(path: &PathBuf, diag: DiagConn) -> EcslResult<EcslConfig> {
        let root_bundle_toml = Self::load_bundle_toml(path)?;

        let mut config = EcslConfig {
            abs_paths: BiHashMap::new(),
            packages: BTreeMap::new(),
            dependency_queue: Vec::new(),
            cycle: None,
        };
        config.include_bundle_toml(root_bundle_toml, diag.clone());

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
        let mut bundle_toml_path = path.clone();
        bundle_toml_path.push(Self::CONFIG_FILE);

        BundleToml::deserialize(&bundle_toml_path)
            .map_err(|e| EcslError::new(ErrorLevel::Error, e.to_string()))
    }

    fn include_bundle_toml(&mut self, bundle_toml: BundleToml, diag: DiagConn) -> CrateID {
        let package_id = self.crate_id_from_path(&bundle_toml.package.path);
        let mut package = EcslPackage::new(package_id, bundle_toml.package.clone());

        for (name, path) in bundle_toml.dependencies {
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
