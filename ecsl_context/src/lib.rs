use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
};

use ecsl_config::{package::PackageInfo, EcslRootConfig};
use ecsl_error::{EcslError, EcslResult, ErrorLevel};
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
    pub fn new(path: PathBuf) -> EcslResult<Self> {
        let config = EcslRootConfig::new_root_config(&path)?;

        if let Some(cycle_causer) = config.cycle {
            return Err(EcslError::new(
                ErrorLevel::Error,
                format!("Cycle caused by {:?} ", config.get_crate(cycle_causer).unwrap().to_string())
            ));
        }

        if config.failed_packages.len() > 0 {
            println!("{:?}", config);
            todo!()
        }

        let mut context = Context {
            config,
            sources: Vec::new(),
            crate_map: BTreeMap::new(),
        };

        context.read_src_dir(&context.config.root().clone())?;

        println!("{:#?}", context);

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
}

#[derive(Debug)]
pub struct SourceCollection {
    pub crate_id: CrateID,
    pub root: PathBuf,
    pub file_map: HashMap<PathBuf, SourceFileID>,
}

#[cfg(test)]
pub mod test {
    use crate::Context;

    #[test]
    pub fn context() {
        Context::new("../example/".into()).unwrap();
    }
}
