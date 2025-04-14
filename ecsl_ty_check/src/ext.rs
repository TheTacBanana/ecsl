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

impl IntoTyID for (TyID, SymbolID) {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        let global_for_tyid = ty_ctxt.global.global_from_tyid(self.0)?;

        if global_for_tyid.source_file() == ty_ctxt.file {
            return match ty_ctxt.get_global_id(Some(global_for_tyid.symbol()), self.1) {
                Some(gid) => gid.into_tyid(ty_ctxt),
                None => Some(TyID::UNKNOWN),
            };
        }

        let resolved = ty_ctxt.imported_resolved.read().unwrap();
        let (None, scope) = resolved.get_by_right(&global_for_tyid)? else {
            return None;
        };

        match ty_ctxt.get_global_id(Some(*scope), self.1) {
            Some(gid) => gid.into_tyid(ty_ctxt),
            None => Some(TyID::UNKNOWN),
        }
    }
}

impl IntoTyID for (Option<SymbolID>, SymbolID) {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        match ty_ctxt.get_global_id(self.0, self.1) {
            Some(gid) => gid.into_tyid(ty_ctxt),
            None => Some(TyID::UNKNOWN),
        }
    }
}
impl IntoTyID for SymbolID {
    fn into_tyid(self, ty_ctxt: &LocalTyCtxt) -> Option<TyID> {
        match ty_ctxt.get_global_id(None, self) {
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
