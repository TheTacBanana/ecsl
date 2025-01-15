use config::EcslConfig;
use ecsl_diagnostics::Diagnostics;
use ecsl_error::{ext::EcslErrorExt, EcslError, EcslResult, ErrorLevel};
use ecsl_index::{CrateID, SourceFileID};
use ecsl_parse::source::SourceFile;
use glob::glob;
use package::EcslPackage;
use rayon::prelude::*;
use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
    sync::Arc,
};

pub mod bundle_toml;
pub mod config;
pub mod package;

pub struct Context {
    pub config: EcslConfig,
    sources: BTreeMap<SourceFileID, SourceFile>,
    source_paths: HashMap<PathBuf, SourceFileID>,
}

pub struct AssocContext<T> {
    pub assoc: BTreeMap<SourceFileID, T>,
}

impl Context {
    pub fn new(
        path: PathBuf,
        diag: Arc<Diagnostics>,
    ) -> EcslResult<(Arc<Context>, AssocContext<()>)> {
        let config = EcslConfig::new_config(&path, diag.clone())?;

        if let Some(cycle_causer) = config.cycle {
            let package_info = &config.get_crate(cycle_causer).unwrap().info;
            diag.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    format!("Dependency cycle caused by crate {}", package_info.name),
                )
                .with_path(|_| package_info.path.clone()),
            );
        }

        let mut context = Context {
            config,
            sources: BTreeMap::new(),
            source_paths: HashMap::new(),
        };

        {
            let packages = std::mem::replace(&mut context.config.packages, BTreeMap::new());
            for (_, conf) in &packages {
                context.read_package(conf);
            }

            context.config.packages = packages;
        }

        let assoc = context.sources.iter().map(|(id, _)| (*id, ())).collect();
        let assoc = AssocContext { assoc };

        Ok((Arc::new(context), assoc))
    }

    fn read_package(&mut self, package: &EcslPackage) {
        let package_info = &package.info;
        let mut root = package_info.path.clone();
        root.push("src/**/*.ecsl");

        let mut file_map: HashMap<PathBuf, SourceFileID> = HashMap::new();
        for entry in glob(root.to_str().unwrap()).unwrap() {
            if let Ok(full_path) = entry {
                let relative_path =
                    PathBuf::from(full_path.strip_prefix(&package_info.path).unwrap());

                let file_id = self.create_source_file(full_path, package.id);

                file_map.insert(relative_path, file_id);
            }
        }
    }

    fn create_source_file(&mut self, full_path: PathBuf, cr: CrateID) -> SourceFileID {
        let next_id = SourceFileID::new(self.sources.len());
        let source = SourceFile::from_path(full_path.clone(), next_id, cr);
        self.sources.insert(next_id, source);
        self.source_paths.insert(full_path, next_id);
        next_id
    }

    pub fn config(&self) -> &EcslConfig {
        &self.config
    }

    pub fn sources(&self) -> impl Iterator<Item = (&SourceFileID, &SourceFile)> {
        self.sources.iter()
    }

    pub fn get_source_file(&self, id: SourceFileID) -> Option<&SourceFile> {
        self.sources.get(&id)
    }

    pub fn get_source_file_package(&self, id: SourceFileID) -> Option<&EcslPackage> {
        let source = self.sources.get(&id)?;
        self.config.packages.get(&source.cr)
    }

    pub fn get_source_file_in_crate(&self, path: &PathBuf, cr: CrateID) -> Option<SourceFileID> {
        let id = self.source_paths.get(path)?;
        let source_file = self.sources.get(&id)?;
        if source_file.cr == cr {
            Some(*id)
        } else {
            None
        }
    }
}

pub trait MapAssocExt<T: Send> {
    fn par_map_assoc<U: Send>(
        self,
        f: impl Fn(&SourceFile, T) -> Option<U> + Send + Sync,
    ) -> Result<AssocContext<U>, ()>;

    fn par_map_assoc_with<U: Send, W: Send + Sync>(
        self,
        w: &W,
        f: impl Fn(&W, &SourceFile, T) -> Option<U> + Send + Sync,
    ) -> Result<AssocContext<U>, ()>;
}

impl<T: Send> MapAssocExt<T> for (&Arc<Context>, AssocContext<T>) {
    fn par_map_assoc<U: Send>(
        self,
        f: impl Fn(&SourceFile, T) -> Option<U> + Send + Sync,
    ) -> Result<AssocContext<U>, ()> {
        let len = self.1.assoc.len();

        let out = self
            .1
            .assoc
            .into_par_iter()
            .filter_map(|(k, v)| {
                let src = self.0.sources.get(&k).unwrap();
                let result = f(src, v);
                result.map(|r| (k, r))
            })
            .collect::<BTreeMap<SourceFileID, U>>();

        if out.len() != len {
            return Err(());
        }

        Ok(AssocContext { assoc: out })
    }

    fn par_map_assoc_with<U: Send, W: Send + Sync>(
        self,
        w: &W,
        f: impl Fn(&W, &SourceFile, T) -> Option<U> + Send + Sync,
    ) -> Result<AssocContext<U>, ()> {
        let len = self.1.assoc.len();

        let out = self
            .1
            .assoc
            .into_par_iter()
            .filter_map(|(k, v)| {
                let src = self.0.sources.get(&k).unwrap();
                let result = f(w, src, v);
                result.map(|r| (k, r))
            })
            .collect::<BTreeMap<SourceFileID, U>>();

        if out.len() != len {
            return Err(());
        }

        Ok(AssocContext { assoc: out })
    }
}
