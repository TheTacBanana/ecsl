use ecsl_bytecode::{Bytecode, BytecodeInstruction, FunctionBytecode, Immediate, Opcode};
use ecsl_gir::{
    expr::{BinOpKind, ExprKind, Operand},
    stmt::StmtKind,
    term::{SwitchCase, TerminatorKind},
    LocalKind, GIR, P,
};
use ecsl_gir_pass::{const_eval::ConstMap, GIRPass};
use ecsl_index::{BlockID, ConstID, LocalID};
use ecsl_ty::local::LocalTyCtxt;
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

        debug!("{:?} {:?}", pre, post);

        // Assign offsets for ret and args
        let mut pre_offset = -8;
        for (i, l) in pre.iter().rev() {
            let size = self.ty_ctxt.global.get_size(l.tyid);
            if size == 0 {
                continue;
            }
            pre_offset -= size as i64;
            debug!("Pre {:?} with offset {:?}", i, pre_offset);
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
            debug!("Post {:?} with offset {:?} size {:?}", i, post_offset, size);
            self.offsets.insert(
                **i,
                StackOffset {
                    offset: post_offset,
                    size,
                },
            );
            post_offset += size as i64;
        }

        // Instruction to set the SP
        let set_sp = BytecodeInstruction::new(Opcode::SETSPR, [Immediate::Long(post_offset)]);

        let mut visited = HashSet::new();
        let mut frontier = vec![BlockID::ZERO];
        let mut next_frontier = Vec::new();
        loop {
            if frontier.is_empty() {
                break;
            }
            debug!("{:?}", frontier);

            for block_id in frontier.iter() {
                if visited.contains(block_id) {
                    continue;
                }
                // Set block as visited
                visited.insert(*block_id);

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
                                            BinOpKind::Eq => {
                                                ins.push(BytecodeInstruction::new(
                                                    Opcode::CMPI,
                                                    [],
                                                ));
                                            }
                                            _ => panic!(),
                                            // BinOpKind::Add => todo!(),
                                            // BinOpKind::Sub => todo!(),
                                            // BinOpKind::Mul => todo!(),
                                            // BinOpKind::Div => todo!(),
                                            // BinOpKind::And => todo!(),
                                            // BinOpKind::Or => todo!(),
                                            // BinOpKind::Neq => todo!(),
                                            // BinOpKind::Lt => todo!(),
                                            // BinOpKind::Leq => todo!(),
                                            // BinOpKind::Gt => todo!(),
                                            // BinOpKind::Geq => todo!(),
                                        }
                                    }
                                    ExprKind::UnOp(un_op_kind, operand) => todo!(),
                                    ExprKind::Reference(mutable, local_id) => todo!(),
                                    ExprKind::Cast(operand, ty_id) => todo!(),
                                    ExprKind::Call(ty_id, operands) => todo!(),
                                }

                                ins.push(self.store_local(*local_id));
                            }
                            StmtKind::ASM(i) => ins.push(i.clone()),
                            StmtKind::Expr(expr) => todo!(),
                        }
                    }

                    match &block.term().kind {
                        TerminatorKind::Jump(block_id) => ins.push(BytecodeInstruction::new(
                            Opcode::JMP,
                            [Immediate::LabelOf(*block_id)],
                        )),
                        TerminatorKind::Return => {
                            ins.push(BytecodeInstruction::new(Opcode::RET, []))
                        }
                        TerminatorKind::Switch(operand, switch_cases) => {
                            for case in switch_cases {
                                ins.push(self.load_operand(*operand));
                                match case {
                                    SwitchCase::Value(value, block_id) => {
                                        ins.push(BytecodeInstruction::new(
                                            Opcode::PSHI,
                                            [Immediate::Int(*value)],
                                        ));
                                        ins.push(BytecodeInstruction::new(Opcode::CMPI, []));
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

        for (_, block) in self.blocks.iter_mut() {
            block.retain(|i| i.op != Opcode::NOP);
        }
        debug!("{:#?}", self.blocks);

        FunctionBytecode {
            tyid: gir.fn_id(),
            ins: Vec::new(),
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
            _ => panic!(),
            // Immediate::UByte(_) => todo!(),
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
