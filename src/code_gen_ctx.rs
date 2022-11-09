use std::collections::HashMap;

use inkwell::{builder::Builder, context::Context, module::Module, values::AnyValueEnum};

pub struct CodeGenCtx<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    named_values: HashMap<String, AnyValueEnum<'ctx>>,
}

impl<'ctx> CodeGenCtx<'ctx> {
    pub fn new(module_name: &str, context: &'ctx Context) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        CodeGenCtx {
            context,
            module,
            builder,
            named_values: HashMap::new(),
        }
    }

    pub fn context(&self) -> &'ctx Context {
        self.context
    }

    pub fn named_values(&self) -> &HashMap<String, AnyValueEnum<'ctx>> {
        &self.named_values
    }

    pub fn named_values_mut(&mut self) -> &mut HashMap<String, AnyValueEnum<'ctx>> {
        &mut self.named_values
    }

    pub fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }
}
