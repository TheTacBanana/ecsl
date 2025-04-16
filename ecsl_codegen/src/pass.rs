use ecsl_bytecode::function::FunctionBytecode;

pub trait CodegenPass {
    type PassInput<'a>;
    type PassResult;

    fn apply_pass<'a>(gir: &mut FunctionBytecode, t: Self::PassInput<'a>) -> Self::PassResult;
}
