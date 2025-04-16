use crate::pass::CodegenPass;
use ecsl_bytecode::{function::FunctionBytecode, BytecodeInstruction, Opcode};

type BI = BytecodeInstruction;
type Op = Opcode;

pub struct BpPromotion;

impl CodegenPass for BpPromotion {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass<'a>(bytecode: &mut FunctionBytecode, _: ()) -> Self::PassResult {
        for (_, block) in bytecode.blocks.iter_mut() {
            let mut out = Vec::new();
            {
                let mut iter = block.drain(..);
                let mut last = None;
                while let Some(mut byt) = iter.next() {
                    match (&last, &byt) {
                        (Some(BI { op: Op::PBP, .. }), BI { op: Op::LDR, .. }) => {
                            _ = last.take();
                            byt.op = Op::BPLDR;
                        }
                        (Some(BI { op: Op::PBP, .. }), BI { op: Op::STR, .. }) => {
                            _ = last.take();
                            byt.op = Op::BPSTR;
                        }
                        _ => (),
                    }

                    if let Some(last) = last {
                        out.push(last);
                    }
                    last = Some(byt);
                }
                if let Some(last) = last {
                    out.push(last);
                }
            }

            *block = out;
        }
    }
}
