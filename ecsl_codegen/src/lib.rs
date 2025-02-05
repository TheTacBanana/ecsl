use ecsl_bytecode::{ext::BytecodeExt, BytecodeInstruction, FunctionBytecode, Immediate, Opcode};
use ecsl_gir::{
    expr::{BinOpKind, ExprKind, Operand},
    stmt::StmtKind,
    term::{SwitchCase, TerminatorKind},
    LocalKind, GIR, P,
};
use ecsl_gir_pass::{const_eval::ConstMap, GIRPass};
use ecsl_index::{BlockID, ConstID, LocalID};
use ecsl_ty::{local::LocalTyCtxt, TyIr};
use log::debug;
use std::{
    collections::{BTreeMap, HashSet},
    sync::Arc,
};

pub struct CodeGen {
    pub ty_ctxt: Arc<LocalTyCtxt>,
    pub const_map: ConstMap,
    pub offsets: BTreeMap<LocalID, StackOffset>,
    pub blocks: BTreeMap<BlockID, Vec<BytecodeInstruction>>,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct StackOffset {
    offset: i64,
    size: usize,
}

impl GIRPass for CodeGen {
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
        };
        c.generate_code(&gir)
    }
}

impl CodeGen {
    pub fn generate_code(&mut self, gir: &GIR) -> FunctionBytecode {
        let mut locals = gir.locals().collect::<Vec<_>>();
        let first_non_arg = locals
            .iter()
            .position(|(_, l)| match l.kind {
                LocalKind::Temp | LocalKind::Let => true,
                _ => false,
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
                    let mut ins = Vec::new();
                    let block = gir.get_block(*block_id).unwrap();
                    for s in block.stmts() {
                        match &s.kind {
                            StmtKind::Assign(local_id, expr) => {
                                match &expr.kind {
                                    ExprKind::Value(operand) => {
                                        ins.push(self.load_operand(*operand));
                                    }
                                    ExprKind::BinOp(bin_op_kind, lhs, rhs) => {
                                        ins.push(self.load_operand(*lhs));
                                        ins.push(self.load_operand(*rhs));

                                        match bin_op_kind {
                                            BinOpKind::Add => {
                                                ins.push(BytecodeInstruction::new(Opcode::ADDI, []))
                                            }
                                            BinOpKind::Sub => {
                                                ins.push(BytecodeInstruction::new(Opcode::SUBI, []))
                                            }
                                            BinOpKind::Mul => {
                                                ins.push(BytecodeInstruction::new(Opcode::MULI, []))
                                            }
                                            BinOpKind::Div => {
                                                ins.push(BytecodeInstruction::new(Opcode::DIVI, []))
                                            }
                                            BinOpKind::Eq => {
                                                ins.push(BytecodeInstruction::new(
                                                    Opcode::CMPI,
                                                    [],
                                                ));
                                            }
                                            _ => panic!(),
                                            // BinOpKind::And => todo!(),
                                            // BinOpKind::Or => todo!(),
                                            // BinOpKind::Neq => todo!(),
                                            // BinOpKind::Lt => todo!(),
                                            // BinOpKind::Leq => todo!(),
                                            // BinOpKind::Gt => todo!(),
                                            // BinOpKind::Geq => todo!(),
                                        }
                                    }
                                    ExprKind::Call(ty_id, operands) => {
                                        let TyIr::Fn(f) = self.ty_ctxt.global.get_tyir(*ty_id)
                                        else {
                                            panic!()
                                        };
                                        let size = self.ty_ctxt.global.get_size(f.ret);
                                        if size > 0 {
                                            ins.push(BytecodeInstruction::new(
                                                Opcode::SETSPR,
                                                [Immediate::Long(size as i64)],
                                            ));
                                        }

                                        for op in operands {
                                            ins.push(self.load_operand(*op));
                                        }
                                        ins.push(BytecodeInstruction::new(
                                            Opcode::CALL,
                                            [Immediate::AddressOf(*ty_id)],
                                        ));
                                        for _ in operands {
                                            ins.push(BytecodeInstruction::new(Opcode::POP, []));
                                        }
                                    }
                                    ExprKind::UnOp(un_op_kind, operand) => todo!(),
                                    ExprKind::Reference(mutable, local_id) => todo!(),
                                    ExprKind::Cast(operand, ty_id) => todo!(),
                                }

                                ins.push(self.store_local(*local_id));
                            }
                            StmtKind::ASM(byt) => {
                                let mut byt = byt.clone();
                                for imm in byt.operand.iter_mut() {
                                    if let Immediate::LocalOf(local_id) = imm {
                                        let offset = self.offsets.get(&local_id).unwrap();
                                        *imm = Immediate::Long(offset.offset)
                                    }
                                }
                                ins.push(byt)
                            }
                            StmtKind::Expr(expr) => todo!(),
                        }
                    }

                    match &block.term().kind {
                        TerminatorKind::Jump(block_id) => ins.push(BytecodeInstruction::new(
                            Opcode::JMP,
                            [Immediate::LabelOf(*block_id)],
                        )),
                        TerminatorKind::Return => {
                            if gir.fn_id() == self.ty_ctxt.global.entry_point() {
                                ins.push(BytecodeInstruction::new(Opcode::HALT, []));
                            } else {
                                ins.push(BytecodeInstruction::new(Opcode::RET, []))
                            }
                        }
                        TerminatorKind::Switch(operand, switch_cases) => {
                            debug!("{:?}", switch_cases);
                            for case in switch_cases {
                                match case {
                                    SwitchCase::Value(value, block_id) => {
                                        ins.push(self.load_operand(*operand));
                                        ins.push(BytecodeInstruction::new(
                                            Opcode::PSHIB,
                                            [Immediate::Byte(*value as i8)],
                                        ));
                                        ins.push(BytecodeInstruction::new(Opcode::CMPB, []));
                                        ins.push(BytecodeInstruction::new(
                                            Opcode::JEZ,
                                            [Immediate::LabelOf(*block_id)],
                                        ));
                                    }
                                    SwitchCase::Default(block_id) => {
                                        ins.push(BytecodeInstruction::new(
                                            Opcode::JMP,
                                            [Immediate::LabelOf(*block_id)],
                                        ))
                                    }
                                }
                            }
                        }
                    }

                    self.blocks.insert(*block_id, ins);
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
        debug!("{:#?}", self.blocks);
        debug!("{:?}", visit_order);
        debug!("{:?}", sizes);
        debug!("{:?}", offsets);

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

    pub fn load_operand(&self, operand: Operand) -> BytecodeInstruction {
        match operand {
            Operand::Copy(local_id) | Operand::Move(local_id) => self.load_local(local_id),
            Operand::Constant(const_id) => self.load_const(const_id),
            Operand::Unknown => panic!(),
        }
    }

    pub fn load_const(&self, cons: ConstID) -> BytecodeInstruction {
        let cons = self.const_map.get(&cons).unwrap();
        match cons {
            Immediate::AddressOf(_) => BytecodeInstruction::new(Opcode::PSHIL, [*cons]),
            Immediate::LabelOf(_) => BytecodeInstruction::new(Opcode::PSHIL, [*cons]),
            Immediate::Int(_) => BytecodeInstruction::new(Opcode::PSHI, [*cons]),
            Immediate::Byte(_) => BytecodeInstruction::new(Opcode::PSHIB, [*cons]),
            Immediate::UByte(_) => BytecodeInstruction::new(Opcode::PSHIB, [*cons]),
            e => panic!("{:?}", e),
            // Immediate::UInt(_) => todo!(),
            // Immediate::Float(_) => todo!(),
            // Immediate::Long(_) => todo!(),
            // Immediate::ULong(_) => todo!(),
            // Immediate::Double(_) => todo!(),
        }
    }

    pub fn load_local(&self, local: LocalID) -> BytecodeInstruction {
        if let Some(stack_offset) = self.offsets.get(&local) {
            BytecodeInstruction::new(Opcode::LDR, [Immediate::Long(stack_offset.offset)])
        } else {
            BytecodeInstruction::new(Opcode::NOP, [])
        }
    }

    pub fn store_local(&self, local: LocalID) -> BytecodeInstruction {
        if let Some(stack_offset) = self.offsets.get(&local) {
            BytecodeInstruction::new(Opcode::STR, [Immediate::Long(stack_offset.offset)])
        } else {
            BytecodeInstruction::new(Opcode::NOP, [])
        }
    }

    // pub fn generate_block(&mut self, gir: &GIR, block: &Block) -> Vec<BytecodeInstruction> {
    //     let mut instructions = Vec::new();
    //     for s in block.stmts() {
    //         match &s.kind {
    //             StmtKind::ASM(ins) => {
    //                 instructions.push(ins.clone());
    //             }
    //             StmtKind::Assign(local_id, expr) => {}
    //             StmtKind::Expr(expr) => todo!(),
    //         }
    //     }
    //     let term = block.term();
    //     instructions.extend(self.generate_term(gir, term));
    //     instructions
    // }

    // pub fn generate_expr(
    //     &mut self,
    //     gir: &GIR,
    //     expr: &Expr,
    // ) -> impl IntoIterator<Item = BytecodeInstruction> {
    //     match expr.kind {
    //         ExprKind::Value(Operand::Constant(const_id)) => {
    //             let c = self.const_map.get(&const_id).unwrap();
    //             BytecodeInstruction::new(Opcode::PSHI, [*c]);
    //         }
    //         ExprKind::Value(Operand::Move(local_id)) => {
    //             panic!()
    //         }
    //         ExprKind::Value(Operand::Copy(local_id)) => {
    //             panic!()
    //         }
    //         ExprKind::Value(Operand::Unknown) => {
    //             panic!()
    //         }
    //         ExprKind::BinOp(bin_op_kind, operand, operand1) => match bin_op_kind {},
    //         ExprKind::UnOp(un_op_kind, operand) => todo!(),
    //         ExprKind::Call(ty_id, operands) => todo!(),

    //         ExprKind::Reference(mutable, local_id) => todo!(),
    //         ExprKind::Cast(operand, ty_id) => todo!(),
    //     }
    // }

    // pub fn generate_term(
    //     &mut self,
    //     gir: &GIR,
    //     term: &Terminator,
    // ) -> impl IntoIterator<Item = BytecodeInstruction> {
    //     match &term.kind {
    //         TerminatorKind::Jump(block_id) => [BytecodeInstruction::new(
    //             Opcode::JRE,
    //             [Immediate::LabelOf(*block_id)],
    //         )],
    //         TerminatorKind::Return => [BytecodeInstruction::new(Opcode::RET, [])],
    //         TerminatorKind::Switch(operand, switch_cases) => {
    //             todo!()
    //         }
    //     }
    // }
}
