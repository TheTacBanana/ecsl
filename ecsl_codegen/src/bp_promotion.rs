use crate::CodegenPass;
use ecsl_bytecode::{function::FunctionBytecode, BytecodeInstruction, Immediate, Opcode};

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

pub struct NoPopPromotion;

impl CodegenPass for NoPopPromotion {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass<'a>(bytecode: &mut FunctionBytecode, _: ()) -> Self::PassResult {
        for (_, block) in bytecode.blocks.iter_mut() {
            let mut out = Vec::new();
            {
                let mut iter = block.drain(..);
                let mut last: Option<BytecodeInstruction> = None;
                while let Some(mut byt) = iter.next() {
                    if let Some(l) = &last {
                        match (&l.op, l.operand.as_slice(), &byt.op, byt.operand.as_slice()) {
                            (
                                Op::BPSTR,
                                [Immediate::UByte(size_l), Immediate::Long(offset_l)],
                                Op::BPLDR,
                                [Immediate::UByte(size_r), Immediate::Long(offset_r)],
                            ) if size_l == size_r && offset_l == offset_r => {
                                _ = last.take();
                                byt.op = Op::BPSTRNP;
                            }
                            _ => (),
                        }
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
