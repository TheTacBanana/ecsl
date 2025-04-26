use cfgrammar::Span;
use cons::Constant;
use ecsl_ast::{parse::FnKind, ty::Mutable};
use ecsl_index::{BlockID, ConstID, FieldID, LocalID, SourceFileID, TyID, VariantID};
use petgraph::prelude::DiGraphMap;
use std::collections::{BTreeMap, BTreeSet};
use stmt::Stmt;
use term::Terminator;

pub mod cons;
pub mod expr;
pub mod stmt;
pub mod term;
pub mod visit;

pub type P<T> = Box<T>;

/// (Control Flow) Graph Intermediate Representation for a given function
#[derive(Debug, Clone)]
pub struct GIR {
    pub span: Span,
    pub fn_id: TyID,
    pub fid: SourceFileID,
    pub fn_kind: FnKind,
    required_components: BTreeSet<TyID>,
    locals: BTreeMap<LocalID, Local>,
    consts: BTreeMap<ConstID, Constant>,
    blocks: BTreeMap<BlockID, Block>,
    block_order: DiGraphMap<BlockID, ()>,
}

impl std::fmt::Display for GIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "FnId: {:?}", self.fn_id)?;

        writeln!(f, "Required Components:")?;
        for tyid in &self.required_components {
            writeln!(f, "  {:?}", tyid)?;
        }
        writeln!(f, "")?;

        writeln!(f, "Locals:")?;
        for (i, local) in &self.locals {
            writeln!(f, "  {} : {}", i, local)?;
        }
        writeln!(f, "")?;

        writeln!(f, "Consts:")?;
        for (i, cons) in &self.consts {
            writeln!(f, "  {} : {}", i, cons)?;
        }
        writeln!(f, "")?;

        for (_, block) in &self.blocks {
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl GIR {
    pub fn new(fn_id: TyID, fn_kind: FnKind, fid: SourceFileID, span: Span) -> Self {
        Self {
            span,
            fn_id,
            fn_kind,
            fid,
            locals: Default::default(),
            consts: Default::default(),
            blocks: Default::default(),
            block_order: Default::default(),
            required_components: Default::default(),
        }
    }

    pub fn new_local(&mut self, local: Local) -> LocalID {
        let id = self
            .locals
            .last_entry()
            .map(|e| *e.key() + LocalID::ONE)
            .unwrap_or(LocalID::ZERO);
        self.locals.insert(id, local);
        id
    }

    pub fn get_local(&self, local: LocalID) -> &Local {
        self.locals.get(&local).unwrap()
    }

    pub fn get_local_mut(&mut self, local: LocalID) -> &mut Local {
        self.locals.get_mut(&local).unwrap()
    }

    pub fn new_constant(&mut self, cons: Constant) -> ConstID {
        let id = self
            .consts
            .last_entry()
            .map(|e| *e.key() + ConstID::ONE)
            .unwrap_or(ConstID::ZERO);
        self.consts.insert(id, cons);
        id
    }

    pub fn get_constant(&self, cons: ConstID) -> &Constant {
        self.consts.get(&cons).unwrap()
    }

    pub fn new_block(&mut self) -> BlockID {
        let id = self
            .blocks
            .last_entry()
            .map(|e| *e.key() + BlockID::ONE)
            .unwrap_or(BlockID::ZERO);
        self.blocks.insert(id, Block::new(id));
        id
    }

    pub fn require_component(&mut self, tyid: TyID) {
        self.required_components.insert(tyid);
    }

    pub fn get_block(&self, block: BlockID) -> Option<&Block> {
        self.blocks.get(&block)
    }

    pub fn get_block_mut(&mut self, block: BlockID) -> Option<&mut Block> {
        self.blocks.get_mut(&block)
    }

    pub fn remove_block(&mut self, block: BlockID) -> Option<Block> {
        self.blocks.remove(&block)
    }

    pub fn blocks(&self) -> impl Iterator<Item = (&BlockID, &Block)> {
        self.blocks.iter()
    }

    pub fn locals(&self) -> impl Iterator<Item = (&LocalID, &Local)> {
        self.locals.iter()
    }

    pub fn with_ordering(&mut self, ordering: DiGraphMap<BlockID, ()>) {
        self.block_order = ordering;
    }

    pub fn ordering(&self) -> &DiGraphMap<BlockID, ()> {
        &self.block_order
    }

    pub fn components(&self) -> impl Iterator<Item = &TyID> {
        self.required_components.iter()
    }
}

// Individual block
#[derive(Debug, Clone)]
pub struct Block {
    id: BlockID,
    stmts: Vec<Stmt>,
    term: Option<Terminator>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.id)?;
        for s in &self.stmts {
            writeln!(f, "\t{}", s)?;
        }
        if let Some(term) = &self.term {
            writeln!(f, "\t{}", term)?
        } else {
            writeln!(f, "\t??",)?
        }
        Ok(())
    }
}

impl Block {
    pub fn new(id: BlockID) -> Self {
        Block {
            id,
            stmts: Vec::new(),
            term: None,
        }
    }

    pub fn push(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }

    pub fn terminate(&mut self, term: Terminator) {
        self.term = Some(term);
    }

    pub fn terminate_no_replace(&mut self, term: Terminator) {
        if self.term.is_none() {
            self.term = Some(term);
        }
    }

    pub fn terminated(&self) -> bool {
        self.term.is_some()
    }

    pub fn empty(&self) -> bool {
        self.stmts.is_empty() && self.term.is_none()
    }

    pub fn len(&self) -> usize {
        self.stmts.len()
    }

    pub fn stmts(&self) -> impl Iterator<Item = &Stmt> {
        self.stmts.iter()
    }

    pub fn remove_after(&mut self, f: impl Fn(&Stmt) -> bool) -> bool {
        let old_stmts = std::mem::take(&mut self.stmts);
        let mut new_stmts = Vec::new();

        for s in old_stmts {
            let out = f(&s);
            new_stmts.push(s);
            if out {
                self.stmts = new_stmts;
                _ = self.term.take();
                return true;
            }
        }
        self.stmts = new_stmts;

        return false;
    }

    pub fn term(&self) -> Option<&Terminator> {
        self.term.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub span: Span,
    pub mutable: Mutable,
    pub tyid: TyID,
    pub kind: LocalKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LocalKind {
    Ret,
    Arg,
    Temp,
    Let,
    Internal,
}

impl LocalKind {
    pub fn promote_to_let(&mut self) -> bool {
        match self {
            LocalKind::Temp | LocalKind::Internal => {
                *self = LocalKind::Let;
                true
            }
            _ => false,
        }
    }

    /// Promote a local kind to a new kind if it is temp
    /// Return true if anything happened
    pub fn promote_from_temp(&mut self, kind: LocalKind) -> bool {
        match self {
            LocalKind::Temp => {
                *self = kind;
                true
            }
            _ => false,
        }
    }

    /// Tests wether a local kind can be referenced
    pub fn can_reference(&self) -> bool {
        match self {
            LocalKind::Arg => true,
            LocalKind::Let => true,
            LocalKind::Internal => true,
            LocalKind::Temp => false, // TODO: Promotion of const literals
            LocalKind::Ret => false,
        }
    }
}

impl std::fmt::Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {:?}", self.kind, self.mutable, self.tyid)
    }
}

impl Local {
    pub fn new(span: Span, mutable: Mutable, tyid: TyID, kind: LocalKind) -> Self {
        Self {
            span,
            mutable,
            tyid,
            kind,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Place {
    pub local: LocalID,
    pub span: Span,
    pub projections: Vec<Projection>,
}

impl Place {
    pub const fn return_location(span: Span) -> Self {
        Self::from_local(LocalID::ZERO, span)
    }

    pub const fn from_local(local: LocalID, span: Span) -> Self {
        Place {
            local,
            span,
            projections: Vec::new(),
        }
    }

    pub fn with_projection(mut self, proj: Projection) -> Self {
        self.projections.push(proj);
        self
    }

    pub fn with_projection_ref(&mut self, proj: Projection) -> &mut Self {
        self.projections.push(proj);
        self
    }

    pub fn projected_tyid(&self, gir: &GIR) -> TyID {
        let mut tyid = gir.get_local(self.local).tyid;
        for proj in &self.projections {
            match proj {
                Projection::Field { new_ty, .. } => tyid = *new_ty,
                Projection::Discriminant { .. } => (),
                Projection::Deref { new_ty } => tyid = *new_ty,
                Projection::ArrayIndex { array_element, .. } => tyid = *array_element,
            }
        }
        return tyid;
    }

    pub fn rewrite_tyid(&mut self, f: impl Fn(&mut TyID)) {
        for proj in self.projections.iter_mut() {
            match proj {
                Projection::Field { ty, new_ty, .. } => {
                    f(ty);
                    f(new_ty);
                }
                Projection::Discriminant { tyid } => f(tyid),
                Projection::Deref { new_ty } => f(new_ty),
                Projection::ArrayIndex { array_element, .. } => f(array_element),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Projection {
    Field {
        ty: TyID,
        vid: VariantID,
        fid: FieldID,
        new_ty: TyID,
    },
    Discriminant {
        tyid: TyID,
    },
    Deref {
        new_ty: TyID,
    },
    ArrayIndex {
        array_element: TyID,
        index: usize,
    },
}

impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.local)?;
        if !self.projections.is_empty() {
            // write!(f, "[")?;
            for proj in self.projections.iter() {
                write!(f, "{}", proj)?;
            }
            // write!(f, "]")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Projection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Projection::Field { fid, .. } => write!(f, ".{}", fid.inner()),
            Projection::Discriminant { .. } => write!(f, "Disc"),
            Projection::Deref { .. } => write!(f, "*"),
            Projection::ArrayIndex { index, .. } => write!(f, "[{}]", index),
        }
    }
}
