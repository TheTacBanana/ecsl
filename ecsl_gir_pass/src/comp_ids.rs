use ecsl_assembler::ComponentDef;
use ecsl_index::{ComponentID, TyID};
use ecsl_ty::ctxt::TyCtxt;
use log::debug;
use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

pub struct ComponentDefinitions {
    pub ctxt: Arc<TyCtxt>,
    pub definitions: RwLock<BTreeMap<TyID, ComponentDef>>,
}

impl ComponentDefinitions {
    pub fn new(ctxt: Arc<TyCtxt>) -> Self {
        let s = Self {
            ctxt,
            definitions: Default::default(),
        };

        s.definitions.write().unwrap().insert(
            TyID::UNKNOWN,
            ComponentDef {
                id: ComponentID::ZERO,
                size: 0,
            },
        );

        s
    }

    pub fn add_component(&self, tyid: TyID) -> ComponentID {
        let mut defs = self.definitions.write().unwrap();
        if let Some(existing) = defs.get(&tyid) {
            existing.id
        } else {
            let new_id = ComponentID::new(defs.len());
            defs.insert(
                tyid,
                ComponentDef {
                    id: new_id,
                    size: self.ctxt.get_size(tyid).unwrap(),
                },
            );
            new_id
        }
    }

    pub fn get_component(&self, tyid: TyID) -> ComponentID {
        self.definitions.read().unwrap().get(&tyid).unwrap().id
    }

    pub fn clone_components(&self) -> Vec<ComponentDef> {
        let definitions = self.definitions.read().unwrap();
        definitions.clone().into_values().collect()
    }
}
