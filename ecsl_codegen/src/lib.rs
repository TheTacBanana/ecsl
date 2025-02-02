use ecsl_bytecode::{Bytecode, BytecodeInstruction, FunctionBytecode, Immediate, Opcode};
use ecsl_gir::{
    expr::{Expr, ExprKind, Operand},
    stmt::StmtKind,
    term::{SwitchCase, Terminator, TerminatorKind},
    Block, GIR,
};
use ecsl_gir_pass::{const_eval::ConstMap, GIRPass};
use ecsl_index::{BlockID, LocalID};
use ecsl_ty::local::LocalTyCtxt;
use log::debug;
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    sync::Arc,
};

pub struct CodeGen {
    ty_ctxt: Arc<LocalTyCtxt>,
    const_map: ConstMap,
    offsets: BTreeMap<LocalID, usize>,
    blocks: BTreeMap<BlockID, Vec<BytecodeInstruction>>,
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
        // FunctionBytecode { tyid: gir.fn_id(), ins: Vec::new() };

        let mut visited = HashSet::new();
        let mut frontier = vec![BlockID::ZERO];
        let mut next_frontier = Vec::new();

        loop {
            for block in frontier.iter() {
                // Set block as visited
                visited.insert(*block);

                // Extend frontier
                let term = gir.get_block(*block).unwrap().term();
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

        todo!()
    }

    pub fn generate_block(&mut self, gir: &GIR, block: &Block) -> Vec<BytecodeInstruction> {
        let mut instructions = Vec::new();
        for s in block.stmts() {
            match &s.kind {
                StmtKind::ASM(ins) => {
                    instructions.push(ins.clone());
                }
                StmtKind::Assign(local_id, expr) => {}
                StmtKind::Expr(expr) => todo!(),
            }
        }
        let term = block.term();
        instructions.extend(self.generate_term(gir, term));
        instructions
    }

    pub fn generate_expr(
        &mut self,
        gir: &GIR,
        expr: &Expr,
    ) -> impl IntoIterator<Item = BytecodeInstruction> {
        match expr.kind {
            ExprKind::Value(Operand::Constant(const_id)) => {
                let c = self.const_map.get(&const_id).unwrap();
                BytecodeInstruction::new(Opcode::PSHI, [*c]);
            }
            ExprKind::Value(Operand::Move(local_id)) => {
                panic!()
            }
            ExprKind::Value(Operand::Copy(local_id)) => {
                panic!()
            }
            ExprKind::Value(Operand::Unknown) => {
                panic!()
            }
            ExprKind::BinOp(bin_op_kind, operand, operand1) => match bin_op_kind {},
            ExprKind::UnOp(un_op_kind, operand) => todo!(),
            ExprKind::Call(ty_id, operands) => todo!(),

            ExprKind::Reference(mutable, local_id) => todo!(),
            ExprKind::Cast(operand, ty_id) => todo!(),
        }
    }

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
