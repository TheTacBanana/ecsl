use ecsl_bytecode::{
    ext::BytecodeExt, ins, BytecodeInstruction, FunctionBytecode, Immediate, Opcode,
};
use ecsl_gir::{
    expr::{BinOp, BinOpKind, ExprKind, Operand, OperandKind, UnOp, UnOpKind},
    stmt::StmtKind,
    term::{SwitchCase, TerminatorKind},
    LocalKind, Place, Projection, GIR,
};
use ecsl_gir_pass::{const_eval::ConstMap, GIRPass};
use ecsl_index::{BlockID, ConstID, LocalID};
use ecsl_ty::{local::LocalTyCtxt, TyIr};
use log::debug;
use std::{
    collections::{BTreeMap, HashSet},
    sync::Arc,
};

pub struct CodeGen<'a> {
    pub ty_ctxt: Arc<LocalTyCtxt>,
    pub const_map: ConstMap,
    pub offsets: BTreeMap<LocalID, StackOffset>,
    pub blocks: BTreeMap<BlockID, Vec<BytecodeInstruction>>,
    pub gir: &'a GIR,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct StackOffset {
    offset: i64,
    size: usize,
}

impl<'b> GIRPass for CodeGen<'b> {
    type PassInput<'a> = (Arc<LocalTyCtxt>, ConstMap);
    type PassResult = FunctionBytecode;

    fn apply_pass<'a>(
        gir: &mut GIR,
        (ty_ctxt, const_map): Self::PassInput<'a>,
    ) -> Self::PassResult {
        let mut c = CodeGen {
            ty_ctxt,
            const_map,
            offsets: Default::default(),
            blocks: Default::default(),
            gir: &gir,
        };
        c.generate_code(&gir)
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_code(&mut self, gir: &GIR) -> FunctionBytecode {
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
            let size = self.ty_ctxt.global.get_size(l.tyid);
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
            let size = self.ty_ctxt.global.get_size(l.tyid);
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

        let mut visit_order = Vec::new();
        let mut visited = HashSet::new();
        let mut frontier = vec![BlockID::ZERO];
        let mut next_frontier = Vec::new();
        loop {
            if frontier.is_empty() {
                break;
            }
            for block_id in frontier.iter() {
                if visited.contains(block_id) {
                    continue;
                }
                // Set block as visited
                visited.insert(*block_id);
                visit_order.push(*block_id);

                {
                    let mut instrs = Vec::new();
                    let block = gir.get_block(*block_id).unwrap();
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

                                            BinOp(OperandKind::Bool, BinOpKind::Eq) => ins!(EQ_B),
                                            BinOp(OperandKind::Bool, BinOpKind::Neq) => ins!(NEQ_B),
                                            BinOp(OperandKind::Bool, BinOpKind::And) => ins!(AND_B),
                                            BinOp(OperandKind::Bool, BinOpKind::Or) => ins!(OR_B),

                                            e => panic!("{:?}", e),
                                        });
                                    }
                                    ExprKind::Call(ty_id, operands) => {
                                        for op in operands {
                                            self.load_operand(op, &mut instrs);
                                        }
                                        instrs.push(ins!(CALL, Immediate::AddressOf(*ty_id)));

                                        let mut argument_size = 0;
                                        for op in operands {
                                            argument_size += self.operand_size(op)
                                        }
                                        if argument_size > 0 {
                                            instrs.push(ins!(
                                                POP,
                                                Immediate::UByte(argument_size as u8)
                                            ));
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
                                    ExprKind::Reference(_, _) => todo!(),
                                }

                                if let Some(place_offset) = self.place_offset(place) {
                                    // debug!("place offset {:?}", place_offset);
                                    instrs.push(ins!(
                                        STR,
                                        Immediate::UByte(place_offset.size as u8),
                                        Immediate::Long(place_offset.offset)
                                    ));
                                }
                            }
                            StmtKind::BYT(byt) => {
                                let mut byt = byt.clone();
                                for imm in byt.operand.iter_mut() {
                                    if let Immediate::LocalOf(local_id) = imm {
                                        let offset = self.offsets.get(&local_id).unwrap();
                                        *imm = Immediate::Long(offset.offset)
                                    }
                                }
                                instrs.push(byt)
                            }
                            StmtKind::Expr(_) => todo!(),
                        }
                    }

                    match &block.term().kind {
                        TerminatorKind::Jump(block_id) => instrs.push(BytecodeInstruction::new(
                            Opcode::JMP,
                            [Immediate::LabelOf(*block_id)],
                        )),
                        TerminatorKind::Return => {
                            if gir.fn_id() == self.ty_ctxt.global.entry_point() {
                                instrs.push(ins!(HALT));
                            } else {
                                instrs.push(ins!(RET));
                            }
                        }
                        TerminatorKind::Switch(operand, switch_cases) => {
                            debug!("{:?}", switch_cases);
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

                    self.blocks.insert(*block_id, instrs);
                }

                // Extend frontier
                let term = gir.get_block(*block_id).unwrap().term();
                match &term.kind {
                    TerminatorKind::Jump(id) => next_frontier.push(*id),
                    TerminatorKind::Switch(_, switch_cases) => {
                        next_frontier.extend(switch_cases.iter().map(|c| match c {
                            SwitchCase::Value(_, id) => *id,
                            SwitchCase::Default(id) => *id,
                        }));
                    }
                    TerminatorKind::Return => (),
                }
            }
            frontier = std::mem::take(&mut next_frontier);
        }

        // Remove all No Ops
        for (_, block) in self.blocks.iter_mut() {
            block.retain(|i| i.op != Opcode::NOP);
        }

        // Instructions
        let mut ins = Vec::new();

        if gir.fn_id() == self.ty_ctxt.global.entry_point() {
            post_offset += 8;
        }

        // Set the SP offset
        if post_offset > 0 {
            ins.push(BytecodeInstruction::new(
                Opcode::SETSP,
                [Immediate::ULong(post_offset as u64)],
            ));
        }

        // Calculate sizes and offsets
        let sizes = self
            .blocks
            .iter()
            .map(|(i, b)| (*i, b.bytecode_size()))
            .collect::<BTreeMap<_, _>>();
        let mut offsets = BTreeMap::new();
        visit_order
            .iter()
            .fold(ins.bytecode_size(), |cur_offset, block_id| {
                offsets.insert(*block_id, cur_offset);
                cur_offset + sizes.get(block_id).unwrap()
            });

        // Concat blocks into sequence
        for block_id in &visit_order {
            let block = self.blocks.remove(block_id).unwrap();
            ins.extend_from_slice(&block);
        }

        FunctionBytecode {
            tyid: gir.fn_id(),
            total_size: ins.bytecode_size(),
            block_offsets: offsets,
            ins,
        }
    }

    pub fn load_operand(&self, operand: &Operand, instrs: &mut Vec<BytecodeInstruction>) {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                if let Some(place_offset) = self.place_offset(&place) {
                    instrs.push(ins!(
                        LDR,
                        Immediate::UByte(place_offset.size as u8),
                        Immediate::Long(place_offset.offset)
                    ));
                }
            }
            Operand::Constant(const_id) => instrs.push(self.load_const(*const_id)),
        }
    }

    pub fn operand_size(&self, operand: &Operand) -> usize {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                if let Some(place_offset) = self.place_offset(&place) {
                    place_offset.size
                } else {
                    todo!()
                }
            }
            Operand::Constant(const_id) => self.const_map.get(const_id).unwrap().size_of(),
        }
    }

    pub fn load_const(&self, cons: ConstID) -> BytecodeInstruction {
        let cons = self.const_map.get(&cons).unwrap();
        match cons {
            Immediate::AddressOf(_) => ins!(PSHI_L, *cons),
            Immediate::LabelOf(_) => ins!(PSHI_L, *cons),
            Immediate::Int(_) => ins!(PSHI, *cons),
            Immediate::Bool(_) => ins!(PSHI_B, *cons),
            Immediate::UByte(_) => ins!(PSHI_B, *cons),
            Immediate::Float(_) => ins!(PSHI, *cons),
            e => panic!("{:?}", e),
            // Immediate::UInt(_) => todo!(),
            // Immediate::Long(_) => todo!(),
            // Immediate::ULong(_) => todo!(),
            // Immediate::Double(_) => todo!(),
        }
    }

    pub fn place_offset(&self, place: &Place) -> Option<StackOffset> {
        let local = self.gir.get_local(place.local);
        let Some(stack_offset) = self.offsets.get(&place.local) else {
            return None;
        };
        let mut stack_offset = *stack_offset;
        stack_offset.size = self.ty_ctxt.global.get_size(local.tyid);

        let mut iter = place.projections.iter();
        while let Some(proj) = iter.next() {
            match proj {
                Projection::Field {
                    ty,
                    vid,
                    fid,
                    new_ty,
                } => {
                    let offset = self.ty_ctxt.global.get_field_offset(*ty, *vid, *fid);
                    let size = self.ty_ctxt.global.get_size(*new_ty);
                    stack_offset.offset += offset as i64;
                    stack_offset.size = size;
                }
                Projection::Discriminant { tyid } => {
                    let TyIr::ADT(adt) = self.ty_ctxt.global.get_tyir(*tyid) else {
                        panic!()
                    };
                    let disc_size = adt.discriminant_size().unwrap();

                    stack_offset.offset = 0;
                    stack_offset.size = disc_size;
                }
            }
        }

        Some(stack_offset)
    }
}
