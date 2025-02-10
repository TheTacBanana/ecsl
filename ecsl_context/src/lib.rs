use config::EcslConfig;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, EcslResult, ErrorLevel};
use ecsl_index::{PackageID, SourceFileID};
use ecsl_parse::source::SourceFile;
use glob::glob;
use log::debug;
use package::EcslPackage;
use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
};

#[cfg(feature = "parallel")]
use rayon::prelude::*;

pub mod bundle_toml;
pub mod config;
pub mod package;

#[derive(Debug)]
pub struct Context {
    config: EcslConfig,
    sources: BTreeMap<SourceFileID, SourceFile>,
    source_paths: HashMap<PathBuf, SourceFileID>,
}

pub struct AssocContext<T> {
    pub assoc: BTreeMap<SourceFileID, T>,
}

impl Context {
    pub fn new(
        path: PathBuf,
        std_path: PathBuf,
        diag: DiagConn,
    ) -> EcslResult<(Context, AssocContext<()>)> {
        let config = EcslConfig::new_config(&path, &std_path, diag.clone())?;

        if let Some(cycle_causer) = config.cycle {
            let package = config.get_package(cycle_causer).unwrap();
            diag.push_error(
                EcslError::new(
                    ErrorLevel::Error,
                    format!("Dependency cycle caused by package {}", package.name()),
                )
                .with_path(|_| package.bundle_toml_path()),
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

        Ok((context, assoc))
    }

    fn read_package(&mut self, package: &EcslPackage) {
        let mut path = package.src_path();
        path.push("**/*.ecsl");

        for entry in glob(path.to_str().unwrap()).unwrap() {
            if let Ok(full_path) = entry {
                _ = self.create_source_file(&full_path, package.id());
            }
        }
    }

    fn create_source_file(&mut self, full_path: &PathBuf, cr: PackageID) -> SourceFileID {
        let full_path = full_path.canonicalize().ok().unwrap();
        let next_id = SourceFileID::new(self.sources.len());

        debug!("Including source file {:?} with id {}", full_path, next_id);
        let source = SourceFile::from_path(full_path.clone(), next_id, cr);
        self.sources.insert(next_id, source);
        self.source_paths.insert(full_path.clone(), next_id);
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

    /// Get a SourceFileID with a relative path from a packages src file
    /// Gets the packages path from the packages id and appends the extension
    pub fn get_source_file_from_package(
        &self,
        relative_path: &PathBuf,
        cr: PackageID,
    ) -> Result<SourceFileID, ImportPathError> {
        let path = self
            .config()
            .get_package(cr)
            .ok_or(ImportPathError::MissingPackage)?
            .file_path(relative_path);
        let id = self
            .source_paths
            .get(&path)
            .ok_or(ImportPathError::PathError)?;
        let source_file = self.sources.get(&id).unwrap();
        if source_file.cr == cr {
            Ok(*id)
        } else {
            Err(ImportPathError::NotInSamePackage)
        }
    }

    pub fn get_source_file_relative(
        &self,
        relative_path: &PathBuf,
        from_src: SourceFileID,
    ) -> Result<SourceFileID, ImportPathError> {
        let from_source = self.get_source_file(from_src).unwrap();
        let mut path = from_source.path.clone();
        path.pop();
        let cr = from_source.cr;
        path.push(relative_path);
        path.set_extension("ecsl");

        let cannon_path = path
            .canonicalize()
            .map_err(|_| ImportPathError::PathError)?;
        let id = self
            .source_paths
            .get(&cannon_path)
            .ok_or(ImportPathError::PathError)?;
        let to_source = self.sources.get(id).unwrap();
        if to_source.cr == cr {
            Ok(*id)
        } else {
            Err(ImportPathError::NotInSamePackage)
        }
    }

    pub fn in_std(&self, id: SourceFileID) -> bool {
        self.get_source_file(id)
            .is_some_and(|s| s.cr == self.config.std_id)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ImportPathError {
    MissingPackage,
    PathError,
    NotInSamePackage,
}

impl std::fmt::Display for ImportPathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ImportPathError::MissingPackage => "Package cannot be resolved",
            ImportPathError::PathError => "Path cannot be resolved",
            ImportPathError::NotInSamePackage => {
                "Importing files from outside of the specified package is not allowed"
            }
        };
        write!(f, "{}", s)
    }
}

pub trait MapAssocExt<T: Send> {
    fn par_map_assoc<U: Send>(
        self,
        f: impl Fn(&Context, &SourceFile, T) -> Option<U> + Send + Sync,
        a: impl FnOnce() -> Result<(), ()>,
    ) -> Result<AssocContext<U>, ()>;

    fn par_map_assoc_with<U: Send, W: Send + Sync>(
        self,
        w: &W,
        f: impl Fn(&W, &SourceFile, T) -> Option<U> + Send + Sync,
    ) -> Result<AssocContext<U>, ()>;
}

impl<T: Send> MapAssocExt<T> for (&Context, AssocContext<T>) {
    fn par_map_assoc<U: Send>(
        self,
        f: impl Fn(&Context, &SourceFile, T) -> Option<U> + Send + Sync,
        a: impl FnOnce() -> Result<(), ()>,
    ) -> Result<AssocContext<U>, ()> {
        let len = self.1.assoc.len();

        #[cfg(not(feature = "parallel"))]
        let iter = self.1.assoc.into_iter();
        #[cfg(feature = "parallel")]
        let iter = self.1.assoc.into_par_iter();
        let out = iter
            .filter_map(|(k, v)| {
                let src = self.0.sources.get(&k).unwrap();
                let result = f(self.0, src, v);
                result.map(|r| (k, r))
            })
            .collect::<BTreeMap<SourceFileID, U>>();

        a()?;

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

        #[cfg(not(feature = "parallel"))]
        let iter = self.1.assoc.into_iter();
        #[cfg(feature = "parallel")]
        let iter = self.1.assoc.into_par_iter();
        let out = iter
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
