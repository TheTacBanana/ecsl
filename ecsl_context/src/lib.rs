use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
};

use ecsl_config::{package::PackageInfo, EcslRootConfig};
use ecsl_diagnostic::Diagnostics;
use ecsl_error::{EcslError, EcslResult, ErrorLevel, ErrorWithPath};
use ecsl_source::SourceFile;
use ecsl_span::{CrateID, SourceFileID};

use glob::glob;

#[derive(Debug)]
pub struct Context {
    config: EcslRootConfig,
    sources: Vec<SourceFile>,
    crate_map: BTreeMap<CrateID, SourceCollection>,
}

impl Context {
    pub fn new(path: PathBuf, diagnostics: &mut Diagnostics) -> EcslResult<Self> {
        let config = EcslRootConfig::new_root_config(&path)?;

        if let Some(cycle_causer) = config.cycle {
            let package_info = config.get_crate(cycle_causer).unwrap();
            diagnostics.push_error(ErrorWithPath::new(
                EcslError::new(
                    ErrorLevel::Error,
                    format!("Dependency cycle caused by crate {}", package_info.name),
                ),
                package_info.path.clone(),
            ));
        }

        if config.failed_packages.len() > 0 {
            for (dependency, error) in &config.failed_packages {
                let required_by = config.get_crate(dependency.required_by).unwrap();

                diagnostics.push_error(ErrorWithPath::new(
                    EcslError::new(
                        ErrorLevel::Error,
                        format!(
                            "Dependency '{}' required by '{}' could not be resolved. {}",
                            dependency.name, required_by.name, error
                        ),
                    ),
                    path.clone(),
                ))
            }
        }

        let mut context = Context {
            config,
            sources: Vec::new(),
            crate_map: BTreeMap::new(),
        };

        context.read_src_dir(&context.config.root().clone())?;

        Ok(context)
    }

    fn read_src_dir(&mut self, package_info: &PackageInfo) -> EcslResult<()> {
        let mut root = package_info.path.clone();
        root.push("src/**/*.ecsl");

        let mut file_map: HashMap<PathBuf, SourceFileID> = HashMap::new();
        for entry in glob(root.to_str().unwrap()).unwrap() {
            if let Ok(full_path) = entry {
                let relative_path =
                    PathBuf::from(full_path.strip_prefix(&package_info.path).unwrap());

                let file_id = self.create_source_file(full_path);

                file_map.insert(relative_path, file_id);
            }
        }

        let crate_id = CrateID::new(self.crate_map.len() as u32);
        let source_collection = SourceCollection {
            crate_id,
            root: package_info.path.clone(),
            file_map,
        };

        self.crate_map.insert(crate_id, source_collection);

        Ok(())
    }

    fn create_source_file(&mut self, full_path: PathBuf) -> SourceFileID {
        let next_id = self.sources.len();
        let source = SourceFile::from_path(full_path, SourceFileID::new(next_id as u32));
        self.sources.push(source);
        SourceFileID::new(next_id as u32)
    }

    pub fn get_source(&self, id: SourceFileID) -> Option<&SourceFile> {
        self.sources.get(*id as usize)
    }
}

#[derive(Debug)]
pub struct SourceCollection {
    pub crate_id: CrateID,
    pub root: PathBuf,
    pub file_map: HashMap<PathBuf, SourceFileID>,
}

#[cfg(test)]
pub mod test {
    use ecsl_diagnostic::Diagnostics;
    use ecsl_error::{EcslError, ErrorLevel, ErrorWithSnippet};
    use ecsl_span::{BytePos, SourceFileID, Span};

    use crate::Context;

    #[test]
    pub fn context() {
        let mut diag = Diagnostics::new();

        let ctx = Context::new("../example/".into(), &mut diag).unwrap();

        let span = Span::new(SourceFileID::new(0), BytePos::new(4), BytePos::new(16));
        let error = EcslError::spanned(ErrorLevel::Error, "Invalid identifier", span);
        let snippet = ctx
            .get_source(SourceFileID::new(0))
            .unwrap()
            .get_snippet(span);

        diag.push_error(ErrorWithSnippet::new(error, snippet));

        if diag.finish_stage().is_err() {
            diag.ok().unwrap();
        }

        panic!()
    }
}
