use ecsl_gir::GIR;
use ecsl_index::TyID;
use ecsl_ty::mono::Mono;
use std::{collections::BTreeMap, sync::Arc};

pub struct FunctionLinker {
    pub mono: Arc<Mono>,
    pub fn_gir: BTreeMap<TyID, GIR>,
}

impl FunctionLinker {
    pub fn new(mono: Arc<Mono>) -> Self {
        FunctionLinker {
            mono,
            fn_gir: Default::default(),
        }
    }

    pub fn add_gir(&mut self, fnid: TyID, gir: GIR) {
        self.fn_gir.insert(fnid, gir);
    }
}
