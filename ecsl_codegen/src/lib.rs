use ecsl_bytecode::{
    ext::BytecodeExt, function::FunctionBytecode, ins, BytecodeInstruction, Immediate, Opcode,
};
use ecsl_gir::{
    expr::{BinOp, BinOpKind, ExprKind, Operand, OperandKind, UnOp, UnOpKind},
    stmt::StmtKind,
    term::{SwitchCase, TerminatorKind},
    LocalKind, Place, Projection, GIR,
};
use ecsl_gir_pass::{comp_ids::ComponentDefinitions, const_eval::ConstMap, GIRPass};
use ecsl_index::{BlockID, LocalID};
use ecsl_ty::{local::LocalTyCtxt, TyIr};
use log::debug;
use petgraph::visit::{Bfs, Dfs};
use std::{collections::BTreeMap, sync::Arc};

pub mod bp_promotion;
pub mod noop;
pub mod pass;

pub struct CodeGen<'a> {
    pub ty_ctxt: Arc<LocalTyCtxt>,
    pub components: Arc<ComponentDefinitions>,
    pub offsets: BTreeMap<LocalID, StackOffset>,
    pub blocks: BTreeMap<BlockID, Vec<BytecodeInstruction>>,
    pub const_map: &'a ConstMap,
    pub gir: &'a GIR,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct StackOffset {
    offset: i64,
    size: usize,
}

impl<'b> GIRPass for CodeGen<'b> {
    type PassInput<'a> = (Arc<LocalTyCtxt>, Arc<ComponentDefinitions>, &'a ConstMap);
    type PassResult = FunctionBytecode;

    fn apply_pass<'a>(
        gir: &mut GIR,
        (ty_ctxt, components, const_map): Self::PassInput<'a>,
    ) -> Self::PassResult {
        let c = CodeGen {
            ty_ctxt,
            components,
            offsets: Default::default(),
            blocks: Default::default(),
            const_map,
            gir: &gir,
        };
        c.generate_code(&gir)
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_code(mut self, gir: &GIR) -> FunctionBytecode {
        debug!("Codegen {:?}", gir.fn_id);
        let mut locals = gir.locals().collect::<Vec<_>>();
        let first_non_arg = locals
            .iter()
            .position(|(_, l)| match l.kind {
                LocalKind::Temp | LocalKind::Let | LocalKind::Internal => true,
                LocalKind::Arg | LocalKind::Ret => false,
            })
            .unwrap_or(locals.len());

        let post = locals.split_off(first_non_arg);
        let pre = locals;

        // Assign offsets for ret and args
        let mut pre_offset = -8;
        for (i, l) in pre.iter().rev() {
            let size = self.ty_ctxt.global.get_size(l.tyid).unwrap();
            if size == 0 {
                continue;
            }
            pre_offset -= size as i64;
            self.offsets.insert(
                **i,
                StackOffset {
                    offset: pre_offset,
                    size,
                },
            );
        }

        // Assign offset for let values
        let mut post_offset = 0;
        for (i, l) in post.iter() {
            let size = self.ty_ctxt.global.get_size(l.tyid).unwrap();
            if size == 0 || l.kind == LocalKind::Temp {
                continue;
            }
            self.offsets.insert(
                **i,
                StackOffset {
                    offset: post_offset,
                    size,
                },
            );
            post_offset += size as i64;
        }

        let mut bfs = Bfs::new(gir.ordering(), BlockID::ZERO);
        while let Some(block_id) = bfs.next(gir.ordering()) {
            let mut instrs = Vec::new();
            let block = gir.get_block(block_id).unwrap();
            for s in block.stmts() {
                match &s.kind {
                    StmtKind::Assign(place, expr) => {
                        match &expr.kind {
                            ExprKind::Value(operand) => {
                                self.load_operand(operand, &mut instrs);
                            }
                            ExprKind::BinOp(op, lhs, rhs) => {
                                self.load_operand(lhs, &mut instrs);
                                self.load_operand(rhs, &mut instrs);

                                use OperandKind as OpKind;
                                instrs.push(match op {
                                    BinOp(OpKind::Int, BinOpKind::Add) => ins!(ADD_I),
                                    BinOp(OpKind::Int, BinOpKind::Sub) => ins!(SUB_I),
                                    BinOp(OpKind::Int, BinOpKind::Mul) => ins!(MUL_I),
                                    BinOp(OpKind::Int, BinOpKind::Div) => ins!(DIV_I),
                                    BinOp(OpKind::Int, BinOpKind::Eq) => ins!(EQ_I),
                                    BinOp(OpKind::Int, BinOpKind::Neq) => ins!(NEQ_I),
                                    BinOp(OpKind::Int, BinOpKind::Lt) => ins!(LT_I),
                                    BinOp(OpKind::Int, BinOpKind::Leq) => ins!(LEQ_I),
                                    BinOp(OpKind::Int, BinOpKind::Gt) => ins!(GT_I),
                                    BinOp(OpKind::Int, BinOpKind::Geq) => ins!(GEQ_I),
                                    BinOp(OpKind::Int, BinOpKind::Mod) => ins!(MOD_I),

                                    BinOp(OpKind::Int, BinOpKind::BAnd) => ins!(AND_I),
                                    BinOp(OpKind::Int, BinOpKind::BOr) => ins!(OR_I),
                                    BinOp(OpKind::Int, BinOpKind::BXor) => ins!(XOR_I),
                                    BinOp(OpKind::Int, BinOpKind::ShiftLeft) => ins!(SHL_I),
                                    BinOp(OpKind::Int, BinOpKind::ShiftRight) => ins!(SHR_I),

                                    BinOp(OpKind::Float, BinOpKind::Add) => ins!(ADD_F),
                                    BinOp(OpKind::Float, BinOpKind::Sub) => ins!(SUB_F),
                                    BinOp(OpKind::Float, BinOpKind::Mul) => ins!(MUL_F),
                                    BinOp(OpKind::Float, BinOpKind::Div) => ins!(DIV_F),
                                    BinOp(OpKind::Float, BinOpKind::Eq) => ins!(EQ_F),
                                    BinOp(OpKind::Float, BinOpKind::Neq) => ins!(NEQ_F),
                                    BinOp(OpKind::Float, BinOpKind::Lt) => ins!(LT_F),
                                    BinOp(OpKind::Float, BinOpKind::Leq) => ins!(LEQ_F),
                                    BinOp(OpKind::Float, BinOpKind::Gt) => ins!(GT_F),
                                    BinOp(OpKind::Float, BinOpKind::Geq) => ins!(GEQ_F),
                                    BinOp(OpKind::Float, BinOpKind::Mod) => ins!(MOD_F),

                                    BinOp(OperandKind::Bool, BinOpKind::Eq) => ins!(EQ_B),
                                    BinOp(OperandKind::Bool, BinOpKind::Neq) => ins!(NEQ_B),
                                    BinOp(OperandKind::Bool, BinOpKind::And) => ins!(AND_B),
                                    BinOp(OperandKind::Bool, BinOpKind::Or) => ins!(OR_B),

                                    e => panic!("{:?}", e),
                                });
                            }
                            ExprKind::Call(ty_id, operands) => {
                                let mut argument_size = 0;
                                for op in operands {
                                    argument_size += self.load_operand(op, &mut instrs);
                                }
                                instrs.push(ins!(CALL, Immediate::AddressOf(*ty_id)));
                                if argument_size > 0 {
                                    instrs.push(ins!(POP, Immediate::UByte(argument_size as u8)));
                                }
                            }
                            ExprKind::UnOp(op, operand) => {
                                self.load_operand(operand, &mut instrs);

                                instrs.push(match op {
                                    UnOp(OperandKind::Int, UnOpKind::Neg) => ins!(NEG_I),
                                    UnOp(OperandKind::Float, UnOpKind::Neg) => ins!(NEG_F),
                                    UnOp(OperandKind::Bool, UnOpKind::Not) => ins!(NOT_B),
                                    e => panic!("{:?}", e),
                                })
                            }
                            ExprKind::Cast(operand, from, to) => {
                                self.load_operand(operand, &mut instrs);
                                instrs.push(match (from, to) {
                                    (OperandKind::Int, OperandKind::Float) => ins!(ITF),
                                    (OperandKind::Float, OperandKind::Int) => ins!(FTI),
                                    e => panic!("{:?}", e),
                                })
                            }
                            ExprKind::Reference(_, place) => {
                                let (mut nav, _, offset) = self.navigate_to_place(place);
                                nav.push(ins!(PSHR, Immediate::Long(offset.unwrap())));
                                instrs.extend(nav);
                            }
                        }

                        self.store_to_place(place, &mut instrs);
                    }
                    StmtKind::AllocReturn(ty_id) => {
                        let size = self.ty_ctxt.global.get_size(*ty_id).unwrap();
                        if size > 0 {
                            instrs.push(ins!(SETSPR, Immediate::Long(size as i64)));
                        }
                    }
                    StmtKind::BYT(byt) => {
                        let mut byt = byt.clone();
                        for imm in byt.operand.iter_mut() {
                            match imm {
                                Immediate::LocalOf(local_id) => {
                                    let offset = self.offsets.get(&local_id).unwrap();
                                    *imm = Immediate::Long(offset.offset)
                                }
                                Immediate::ComponentOf(tyid) => {
                                    *imm = Immediate::UInt(
                                        self.components.get_component(*tyid).inner() as u32,
                                    )
                                }
                                Immediate::SizeOf(tyid) => {
                                    *imm = Immediate::UByte(
                                        self.ty_ctxt.global.get_size(*tyid).unwrap() as u8,
                                    )
                                }
                                _ => (),
                            }
                        }
                        instrs.push(byt)
                    }
                }
            }

            if let Some(term) = block.term() {
                match &term.kind {
                    TerminatorKind::Jump(block_id) => instrs.push(BytecodeInstruction::new(
                        Opcode::JMP,
                        [Immediate::LabelOf(*block_id)],
                    )),
                    TerminatorKind::Return => {
                        if gir.fn_id == self.ty_ctxt.global.entry_point() {
                            instrs.push(ins!(HALT));
                        } else {
                            instrs.push(ins!(RET));
                        }
                    }
                    TerminatorKind::Switch(operand, switch_cases) => {
                        for case in switch_cases {
                            match case {
                                SwitchCase::Value(value, block_id) => {
                                    self.load_operand(operand, &mut instrs);

                                    instrs.extend(match value {
                                        Immediate::Bool(_) => {
                                            vec![ins!(JMPT, Immediate::LabelOf(*block_id))]
                                        }
                                        Immediate::Int(_) => vec![
                                            ins!(PSHI, *value),
                                            ins!(EQ_I),
                                            ins!(JMPT, Immediate::LabelOf(*block_id)),
                                        ],
                                        Immediate::Float(_) => vec![
                                            ins!(PSHI, *value),
                                            ins!(EQ_F),
                                            ins!(JMPT, Immediate::LabelOf(*block_id)),
                                        ],
                                        Immediate::UByte(_) => vec![
                                            ins!(PSHI_B, *value),
                                            ins!(EQ_B),
                                            ins!(JMPT, Immediate::LabelOf(*block_id)),
                                        ],
                                        e => panic!("{:?}", e),
                                    })
                                }
                                SwitchCase::Default(block_id) => {
                                    instrs.push(ins!(JMP, Immediate::LabelOf(*block_id)))
                                }
                            }
                        }
                    }
                }
            }

            self.blocks.insert(block_id, instrs);
        }

        // Use a dfs preorder
        let mut dfs = Dfs::new(gir.ordering(), BlockID::ZERO);
        let mut visit_order = Vec::new();
        while let Some(node) = dfs.next(gir.ordering()) {
            visit_order.push(node);
        }
        assert!(visit_order.first().is_none_or(|s| *s == BlockID::ZERO));

        if gir.fn_id == self.ty_ctxt.global.entry_point() {
            post_offset += 8;
        }

        // Set the SP offset
        if post_offset > 0 {
            if let Some(block) = self.blocks.get_mut(&BlockID::ZERO) {
                block.insert(
                    0,
                    BytecodeInstruction::new(Opcode::SETSP, [Immediate::ULong(post_offset as u64)]),
                );
            }
        }

        FunctionBytecode {
            tyid: gir.fn_id,
            visit_order,
            blocks: self.blocks,
        }
    }

    /// Loads the operand and returns the size of the value on the stack
    /// Outputs instructions via the mutable reference
    pub fn load_operand(&self, operand: &Operand, instrs: &mut Vec<BytecodeInstruction>) -> usize {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let (mut nav, size, offset) = self.navigate_to_place(place);

                if let Some(offset) = offset {
                    nav.push(ins!(
                        LDR,
                        Immediate::UByte(size as u8),
                        Immediate::Long(offset)
                    ));

                    instrs.extend(nav);
                }
                return size;
            }
            Operand::Constant(const_id) => {
                let cons = self.const_map.get(&const_id).unwrap();
                let ins = match cons {
                    Immediate::AddressOf(_) => ins!(PSHI_L, *cons),
                    Immediate::ConstAddressOf(_) => ins!(PSHI_L, *cons),
                    Immediate::LabelOf(_) => ins!(PSHI_L, *cons),
                    Immediate::Int(_) => ins!(PSHI, *cons),
                    Immediate::Bool(_) => ins!(PSHI_B, *cons),
                    Immediate::UByte(_) => ins!(PSHI_B, *cons),
                    Immediate::Float(_) => ins!(PSHI, *cons),
                    e => panic!("{:?}", e),
                };
                instrs.push(ins);
                return cons.size_of();
            }
        }
    }

    pub fn store_to_place(&self, place: &Place, instrs: &mut Vec<BytecodeInstruction>) {
        let (mut nav, size, offset) = self.navigate_to_place(place);

        if let Some(offset) = offset {
            nav.push(ins!(
                STR,
                Immediate::UByte(size as u8),
                Immediate::Long(offset)
            ));

            instrs.extend(nav);
        }
    }

    /// Returns a vec of instructions to navigate to the place
    /// The size of the final value
    /// And the offset if it exits
    pub fn navigate_to_place(
        &self,
        place: &Place,
    ) -> (Vec<BytecodeInstruction>, usize, Option<i64>) {
        let mut instrs = Vec::new();

        let projected_tyid = place.projected_tyid(&self.gir);

        let Some(stack_offset) = self.offsets.get(&place.local).copied() else {
            if let Some(Projection::Discriminant { .. }) = place.projections.last() {
                panic!("Internal Compiler Error: Assumption that Enums have LocalKind::Internal has been broken")
            }
            return (
                instrs,
                self.ty_ctxt.global.get_size(projected_tyid).unwrap(),
                None,
            );
        };

        let mut cur_stack_offset = stack_offset;

        // let mut ldr_stack = Vec::new();
        instrs.push(ins!(PBP));

        let mut iter = place.projections.iter();
        while let Some(proj) = iter.next() {
            match proj {
                Projection::Field {
                    ty,
                    vid,
                    fid,
                    new_ty,
                } => {
                    let offset = self
                        .ty_ctxt
                        .global
                        .get_field_offset(*ty, *vid, *fid)
                        .unwrap();
                    let size = self.ty_ctxt.global.get_size(*new_ty).unwrap();
                    cur_stack_offset.offset += offset as i64;
                    cur_stack_offset.size = size;
                }
                Projection::Discriminant { tyid } => {
                    let TyIr::ADT(adt) = self.ty_ctxt.global.get_tyir(*tyid) else {
                        panic!()
                    };

                    cur_stack_offset.size = adt.discriminant_size().unwrap();
                }
                Projection::Deref { new_ty } => {
                    instrs.extend(vec![ins!(
                        LDR,
                        Immediate::UByte(8),
                        Immediate::Long(cur_stack_offset.offset)
                    )]);

                    cur_stack_offset.offset = 0;
                    cur_stack_offset.size = self.ty_ctxt.global.get_size(*new_ty).unwrap();
                }
            }
        }

        return (instrs, cur_stack_offset.size, Some(cur_stack_offset.offset));
    }
}
