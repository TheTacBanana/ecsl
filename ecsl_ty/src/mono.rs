use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    sync::RwLock,
};

use bimap::BiBTreeMap;
use ecsl_index::TyID;
use log::debug;

#[derive(Debug, Default)]
pub struct Mono {
    /// Mapping of tyid and concrete generics to monomorphized tyid
    pub mono_map: RwLock<BiBTreeMap<(TyID, Vec<TyID>), TyID>>,

    /// Function which needs to be monomorphized
    pub function_variants: RwLock<BTreeMap<TyID, BTreeSet<TyID>>>,
}

impl Mono {
    pub fn insert_mapping(&self, key: (TyID, Vec<TyID>), value: TyID) {
        debug!("before");
        let tyid = key.0;
        {
            let mut mono_map = self.mono_map.write().unwrap();
            mono_map.insert(key, value);
        }
        debug!("after");
        {
            let mut variants = self.function_variants.write().unwrap();
            match variants.entry(tyid) {
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(vec![value].drain(..).collect());
                }
                Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().insert(value);
                }
            }
        }
        debug!("after");
    }

    pub fn insert(&self, key: (TyID, Vec<TyID>), value: TyID) {
        let mut mono_map = self.mono_map.write().unwrap();
        mono_map.insert(key, value);
    }

    pub fn take_variants(&self, tyid: TyID) -> Option<Vec<(TyID, Vec<TyID>)>> {
        let mono_map = self.mono_map.read().unwrap();
        let mut variants = self.function_variants.write().unwrap();

        if let Some(variants) = variants.remove(&tyid) {
            let mut vec = variants.iter().collect::<Vec<_>>();
            return Some(
                vec.drain(..)
                    .map(|tyid| (*tyid, mono_map.get_by_right(tyid).unwrap().1.clone()))
                    .collect(),
            );
        }
        return None;
    }
}
