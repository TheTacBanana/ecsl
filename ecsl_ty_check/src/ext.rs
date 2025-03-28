use ecsl_ast::ty::Ty;
use ecsl_index::{GlobalID, SymbolID, TyID};
use ecsl_ty::{local::LocalTyCtxt, GenericsScope, TyIr};

pub trait IntoTyID {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID>;
}

impl IntoTyID for TyIr {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        Some(ty_ctxt.global.tyid_from_tyir(self))
    }
}

impl IntoTyID for TyID {
    fn into_tyid(self, _: &LocalTyCtxt) -> Option<TyID> {
        Some(self)
    }
}

impl IntoTyID for GlobalID {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        Some(ty_ctxt.global.get_or_create_tyid(self))
    }
}

impl IntoTyID for SymbolID {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        match ty_ctxt.get_global_id(self) {
            Some(gid) => gid.into_tyid(ty_ctxt),
            None => Some(TyID::UNKNOWN),
        }
    }
}

impl IntoTyID for (&Ty, &GenericsScope) {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        ty_ctxt.get_tyid(self.0, self.1)
    }
}
