use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
};

use ecsl_config::{package::PackageInfo, EcslRootConfig};
use ecsl_diagnostics::Diagnostics;
use ecsl_error::{ext::EcslErrorExt, EcslError, EcslResult, ErrorLevel};
use ecsl_source::SourceFile;
use ecsl_index::{CrateID, SourceFileID};

use glob::glob;

#[derive(Debug)]
pub struct Context {
    config: EcslRootConfig,
    sources: Vec<SourceFile>,
    crate_map: BTreeMap<CrateID, SourceCollection>,
}

impl Context {
    pub fn new(path: PathBuf, diag: &mut Diagnostics) -> EcslResult<Self> {
        let config = EcslRootConfig::new_root_config(&path)?;

        if let Some(cycle_causer) = config.cycle {
            let package_info = config.get_crate(cycle_causer).unwrap();
            diag.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    format!("Dependency cycle caused by crate {}", package_info.name),
                )
                .with_path(|_| package_info.path.clone()),
            );
        }

        if config.failed_packages.len() > 0 {
            for (dependency, error) in &config.failed_packages {
                let required_by = config.get_crate(dependency.required_by).unwrap();
                diag.push_error(
                    EcslError::new(
                        ErrorLevel::Error,
                        format!(
                            "Dependency '{}' required by '{}' could not be resolved. {}",
                            dependency.name, required_by.name, error
                        ),
                    )
                    .with_path(|_| path.clone()),
                )
            }
        }

        let mut context = Context {
            config,
            sources: Vec::new(),
            crate_map: BTreeMap::new(),
        };

        {
            let packages = std::mem::replace(&mut context.config.packages, Vec::new());

            for (i, conf) in packages.iter().enumerate() {
                context.read_src_dir(conf, CrateID::new(i), diag);
            }

            context.config.packages = packages;
        }

        Ok(context)
    }

    fn read_src_dir(&mut self, package_info: &PackageInfo, id: CrateID, diag: &mut Diagnostics) {
        let mut root = package_info.path.clone();
        root.push("src/**/*.ecsl");

        let mut file_map: HashMap<PathBuf, SourceFileID> = HashMap::new();
        for entry in glob(root.to_str().unwrap()).unwrap() {
            if let Ok(full_path) = entry {
                let relative_path =
                    PathBuf::from(full_path.strip_prefix(&package_info.path).unwrap());

                let file_id = self.create_source_file(full_path, diag);

                file_map.insert(relative_path, file_id);
            }
        }

        let source_collection = SourceCollection {
            crate_id: id,
            root: package_info.path.clone(),
            file_map,
        };

        self.crate_map.insert(id, source_collection);
    }

    fn create_source_file(
        &mut self,
        full_path: PathBuf,
        diagnostics: &mut Diagnostics,
    ) -> SourceFileID {
        let next_id = self.sources.len();
        let source =
            SourceFile::from_path(diagnostics, full_path, SourceFileID::new(next_id));
        self.sources.push(source);
        SourceFileID::new(next_id )
    }

    pub fn get_source(&self, id: SourceFileID) -> Option<&SourceFile> {
        self.sources.get(id.inner())
    }

    pub fn source_files(&self) -> &Vec<SourceFile> {
        &self.sources
    }
}

#[derive(Debug)]
pub struct SourceCollection {
    pub crate_id: CrateID,
    pub root: PathBuf,
    pub file_map: HashMap<PathBuf, SourceFileID>,
}
