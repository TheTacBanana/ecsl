use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use ecsl_gir::GIR;
use ecsl_index::TyID;
use ecsl_ty::ctxt::TyCtxt;

pub struct FunctionLinker {
    ctxt: Arc<TyCtxt>,
    fn_gir: RwLock<BTreeMap<TyID, GIR>>,
    mono_queue: RwLock<Vec<TyID>>,
}

impl FunctionLinker {
    pub fn new(ctxt: Arc<TyCtxt>) -> Self {
        FunctionLinker {
            ctxt,
            fn_gir: Default::default(),
            mono_queue: Default::default(),
        }
    }

    pub fn add_gir(&self, fnid: TyID, gir: GIR) {
        let mut lock = self.fn_gir.write().unwrap();
        lock.insert(fnid, gir);
    }

    pub fn queue_mono(&self, fnid: TyID) {
        let mut lock = self.mono_queue.write().unwrap();
        lock.push(fnid);
    }
}
