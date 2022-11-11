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
    let res = run_func(module, function, args);

    // Clear the function.
    // SAFETY: The function is not used anymore.
    unsafe { function.delete() };

    res
}

unsafe fn run_func<'ctx>(
    module: &Module<'ctx>,
    function: FunctionValue<'ctx>,
    args: &[&GenericValue<'ctx>],
) -> Result<GenericValue<'ctx>, Error> {
    let undef_functions = undef_functions(module);

    if undef_functions.len() > 0 {
        return Err(Error::UndefFunctionsError(undef_functions));
    }

    // SAFETY: The execution engine takes ownership of the module.
    let execution_engine = match module.create_execution_engine() {
        Ok(execution_engine) => execution_engine,
        Err(e) => return Err(Error::CreateExecutionEngineError(e.to_string())),
    };

    // SAFETY: Undefined functions are checked above.
    let value = execution_engine.run_function(function, args);

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
    UndefFunctionsError(Vec<String>),
}

fn undef_functions(module: &Module) -> Vec<String> {
    let mut undef_functions = Vec::new();
    for function in module.get_functions() {
        if function.count_basic_blocks() == 0 {
            undef_functions.push(function.get_name().to_str().unwrap().to_string());
        }
    }
    undef_functions
}
