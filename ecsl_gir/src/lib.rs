use std::collections::BTreeMap;

use cfgrammar::Span;
use cons::Constant;
use ecsl_ast::ty::Mutable;
use ecsl_index::{BlockID, ConstID, LocalID, TyID};
use stmt::Stmt;
use term::Terminator;

pub mod cons;
pub mod expr;
pub mod stmt;
pub mod term;
pub mod visit;

pub type P<T> = Box<T>;

/// (Control Flow) Graph Intermediate Representation for a given function
#[derive(Debug)]
pub struct GIR {
    fn_id: TyID,
    locals: BTreeMap<LocalID, Local>,
    consts: BTreeMap<ConstID, Constant>,
    blocks: Vec<Block>,
}

impl std::fmt::Display for GIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

        for block in &self.blocks {
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl GIR {
    pub fn new(fn_id: TyID) -> Self {
        Self {
            fn_id,
            locals: Default::default(),
            consts: Default::default(),
            blocks: Default::default(),
        }
    }

    pub fn fn_id(&self) -> TyID {
        self.fn_id
    }

    pub fn new_local(&mut self, local: Local) -> LocalID {
        let id = LocalID::new(self.locals.len());
        self.locals.insert(id, local);
        id
    }

    pub fn get_local(&self, local: LocalID) -> &Local {
        self.locals.get(&local).unwrap()
    }

    pub fn new_constant(&mut self, cons: Constant) -> ConstID {
        let id = ConstID::new(self.consts.len());
        self.consts.insert(id, cons);
        id
    }

    pub fn get_constant(&self, cons: ConstID) -> &Constant {
        self.consts.get(&cons).unwrap()
    }

    pub fn new_block(&mut self) -> BlockID {
        let id = BlockID::new(self.blocks.len());
        self.blocks.push(Block::new(id));
        id
    }

    pub fn get_block(&self, block: BlockID) -> Option<&Block> {
        self.blocks.get(block.inner())
    }

    pub fn get_block_mut(&mut self, block: BlockID) -> Option<&mut Block> {
        self.blocks.get_mut(block.inner())
    }
}

// Individual block
#[derive(Debug)]
pub struct Block {
    id: BlockID,
    // parents: Vec<BlockID>,
    stmts: Vec<Stmt>,
    term: Option<Terminator>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Block {}:", self.id)?;
        for s in &self.stmts {
            writeln!(f, "  {}", s)?;
        }
        if let Some(term) = &self.term {
            writeln!(f, "  {}", term)?
        } else {
            writeln!(f, "  ??",)?
        }
        Ok(())
    }
}

impl Block {
    pub fn new(id: BlockID) -> Self {
        Block {
            id,
            // parents: Vec::new(),
            stmts: Vec::new(),
            term: None,
        }
    }

    // pub fn add_parent(&mut self, id: BlockID) {
    //     self.parents.push(id);
    // }

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
}

#[derive(Debug)]
pub struct Local {
    pub span: Span,
    pub mutable: Mutable,
    pub tyid: TyID,
}

impl std::fmt::Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.mutable, self.tyid)
    }
}

impl Local {
    pub fn new(span: Span, mutable: Mutable, tyid: TyID) -> Self {
        Self {
            span,
            mutable,
            tyid,
        }
    }
}
