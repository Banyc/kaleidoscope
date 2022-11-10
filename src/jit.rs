use inkwell::{
    module::Module,
    values::{FunctionValue, GenericValue},
};

/// # Safety
///
/// The caller must ensure that the generated code cannot mess with the heap memory.
pub unsafe fn run_and_drop_func<'ctx>(
    module: &Module<'ctx>,
    function: FunctionValue<'ctx>,
    args: &[&GenericValue<'ctx>],
) -> Result<GenericValue<'ctx>, Error> {
    // SAFETY: The execution engine takes ownership of the module.
    let execution_engine = match module.create_execution_engine() {
        Ok(execution_engine) => execution_engine,
        Err(e) => return Err(Error::CreateExecutionEngineError(e.to_string())),
    };

    let value = execution_engine.run_function(function, args);

    // Clear the top-level function.
    // SAFETY: The function is not used anymore.
    unsafe { function.delete() };

    // Clear the execution engine.
    // SAFETY: The execution engine release the module.
    match execution_engine.remove_module(module) {
        Ok(_) => (),
        Err(e) => return Err(Error::ReleaseModuleError(e.to_string())),
    }

    Ok(value)
}

#[derive(Debug)]
pub enum Error {
    CreateExecutionEngineError(String),
    ReleaseModuleError(String),
}
