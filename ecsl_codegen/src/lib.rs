use ecsl_bytecode::function::FunctionBytecode;

pub mod bp_promotion;
pub mod codegen;
pub mod inline;
pub mod noop;

pub trait CodegenPass {
    type PassInput<'a>;
    type PassResult;

    fn apply_pass<'a>(gir: &mut FunctionBytecode, t: Self::PassInput<'a>) -> Self::PassResult;
}
