use inkwell::{
    module::Linkage,
    types::BasicMetadataTypeEnum,
    values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum},
};

use crate::ast::{AnyAst, ExprAst, FunctionAst, PrototypeAst};

use super::module_ctx::ModuleCtx;

pub trait Visitor {
    fn code_gen<'ctx>(&self, ctx: &mut ModuleCtx<'ctx>) -> Result<AnyValueEnum<'ctx>, String>;
}

impl Visitor for AnyAst {
    fn code_gen<'ctx>(&self, ctx: &mut ModuleCtx<'ctx>) -> Result<AnyValueEnum<'ctx>, String> {
        match self {
            AnyAst::Expr(expr) => expr.code_gen(ctx),
            AnyAst::Prototype(prototype) => prototype.code_gen(ctx),
            AnyAst::Function(function) => function.code_gen(ctx),
        }
    }
}

impl Visitor for ExprAst {
    fn code_gen<'ctx>(&self, ctx: &mut ModuleCtx<'ctx>) -> Result<AnyValueEnum<'ctx>, String> {
        match self {
            ExprAst::Number(n) => {
                // float point
                let value = ctx.context().f64_type().const_float(*n);
                Ok(AnyValueEnum::FloatValue(value))
            }
            ExprAst::Variable(variable) => {
                let Some(value) = ctx.named_values().get(variable) else {
                    return Err(format!("Unknown variable name: {}", variable));
                };
                Ok(value.clone())
            }
            ExprAst::Binary { op, lhs, rhs } => {
                let lhs = lhs.code_gen(ctx)?;
                let rhs = rhs.code_gen(ctx)?;

                let lhs = lhs.into_float_value();
                let rhs = rhs.into_float_value();

                match op.as_str() {
                    "+" => {
                        let value = ctx.builder().build_float_add(lhs, rhs, "addtmp");
                        Ok(AnyValueEnum::FloatValue(value))
                    }
                    "-" => {
                        let value = ctx.builder().build_float_sub(lhs, rhs, "subtmp");
                        Ok(AnyValueEnum::FloatValue(value))
                    }
                    "*" => {
                        let value = ctx.builder().build_float_mul(lhs, rhs, "multmp");
                        Ok(AnyValueEnum::FloatValue(value))
                    }
                    "<" => {
                        let value = ctx.builder().build_float_compare(
                            inkwell::FloatPredicate::ULT,
                            lhs,
                            rhs,
                            "cmptmp",
                        );

                        // Convert bool 0/1 to double 0.0 or 1.0
                        let value = ctx.builder().build_unsigned_int_to_float(
                            value,
                            ctx.context().f64_type(),
                            "booltmp",
                        );

                        Ok(AnyValueEnum::FloatValue(value))
                    }
                    _ => Err(format!("Unknown operator: {}", op)),
                }
            }
            ExprAst::Call { callee, args } => {
                let Some(callee) = ctx.module().get_function(callee.as_str()) else {
                    return Err(format!("Unknown function referenced: {}", callee));
                };

                // Check the number of arguments.
                if callee.count_params() as usize != args.len() {
                    return Err(format!(
                        "Incorrect number of arguments passed: {}",
                        args.len()
                    ));
                }

                let args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| {
                        let arg = match arg.code_gen(ctx) {
                            Ok(arg) => arg,
                            Err(err) => return Err(err),
                        };
                        Ok(BasicMetadataValueEnum::FloatValue(arg.into_float_value()))
                    })
                    .collect();
                let args = args?;

                let value = ctx.builder().build_call(callee, &args, "calltmp");
                Ok(value.as_any_value_enum())
            }
            ExprAst::If { cond, then, else_ } => {
                let cond = cond.code_gen(ctx)?;
                let cond = cond.into_float_value();

                // Convert condition to a bool by comparing non-equal to 0.0.
                let cond = ctx.builder().build_float_compare(
                    inkwell::FloatPredicate::ONE,
                    cond,
                    ctx.context().f64_type().const_float(0.0),
                    "ifcond",
                );

                // Get the block that the builder position is currently in.
                let cond_block = match ctx.builder().get_insert_block() {
                    Some(v) => v,
                    None => {
                        return Err("If statement must be in a block".to_string());
                    }
                };

                let function = cond_block.get_parent().unwrap();

                // Append labels to the function.
                let then_block = ctx.context().append_basic_block(function, "then"); // "then": label name
                let else_block = ctx.context().append_basic_block(function, "else");
                let merge_block = ctx.context().append_basic_block(function, "ifcont");

                // Append the conditional branch to the `cond_block` block.
                // `then_block` and `else_block` are for generating the branch label names.
                ctx.builder()
                    .build_conditional_branch(cond, then_block, else_block);

                // Construct the `then_block` labeled "then".
                let then = {
                    // Move position pointer to the end of the `then_block` block so that the following code insertion will be inserted into the `then_block` block.
                    ctx.builder().position_at_end(then_block);

                    // Append the `then` expression to the `then_block` block.
                    let then = then.code_gen(ctx)?;

                    // Append a jump to the `merge_block` block.
                    ctx.builder().build_unconditional_branch(merge_block);

                    then.into_float_value()
                };

                // Construct the `else_block` labeled "else".
                let else_ = {
                    // Move position pointer to the end of the `else_block` block so that the following code insertion will be inserted into the `else_block` block.
                    ctx.builder().position_at_end(else_block);

                    // Append the `else_` expression to the `else_block` block.
                    let else_ = else_.code_gen(ctx)?;

                    // Append a jump to the `merge_block` block.
                    ctx.builder().build_unconditional_branch(merge_block);

                    else_.into_float_value()
                };

                // Construct the `merge_block` labeled "ifcont".
                {
                    // Move position pointer to the end of the `merge_block` block so that the following code insertion will be inserted into the `merge_block` block.
                    ctx.builder().position_at_end(merge_block);

                    // Append the PHI node to the `merge_block` block.
                    let phi = ctx.builder().build_phi(ctx.context().f64_type(), "iftmp");

                    // Add the incoming values to the PHI node by appending them to the `merge_block` block.
                    // `then_block` and `else_block` are provided for the builder to write down the labels of the previous blocks.
                    // If the previous block is `then_block`, the value of `phi` is `then`.
                    // If the previous block is `else_block`, the value of `phi` is `else_`.
                    phi.add_incoming(&[(&then, then_block), (&else_, else_block)]);

                    Ok(AnyValueEnum::FloatValue(
                        phi.as_basic_value().into_float_value(),
                    ))
                }
            }
        }
    }
}

impl Visitor for PrototypeAst {
    fn code_gen<'ctx>(&self, ctx: &mut ModuleCtx<'ctx>) -> Result<AnyValueEnum<'ctx>, String> {
        if ctx.module().get_function(self.name.as_str()).is_some() {
            return Err(format!("Function {} already exists", self.name));
        }

        // Make the function type: double(double, double) etc.

        // (double, double)
        let double_type = BasicMetadataTypeEnum::FloatType(ctx.context().f64_type());
        let doubles = vec![double_type; self.args.len()];

        // double(double, double)
        // is_var_args: true if this function takes a variable number of arguments.
        let fn_type = ctx.context().f64_type().fn_type(&doubles, false);

        // External linkage:
        // - the function may be defined outside the current module
        // - it is callable by functions outside the module
        // The function name is registered in the module's symbol table
        let function =
            ctx.module()
                .add_function(self.name.as_str(), fn_type, Some(Linkage::External));

        Ok(AnyValueEnum::FunctionValue(function))
    }
}

impl Visitor for FunctionAst {
    fn code_gen<'ctx>(&self, ctx: &mut ModuleCtx<'ctx>) -> Result<AnyValueEnum<'ctx>, String> {
        // First, check for an existing function from a previous 'extern' declaration.
        let function = match ctx.module().get_function(self.prototype.name.as_str()) {
            Some(function) => {
                // Validate function signature arg count
                if function.count_params() as usize != self.prototype.args.len() {
                    return Err(format!(
                        "redefinition of function with different # of args: {}",
                        self.prototype.name
                    ));
                }

                function
            }
            None => {
                let prototype = self.prototype.code_gen(ctx)?;
                prototype.into_function_value()
            }
        };

        // Function cannot be redefined.
        if function.count_basic_blocks() != 0 {
            return Err(format!(
                "Function {} cannot be redefined.",
                self.prototype.name
            ));
        }

        // Create a new basic block to start insertion into.
        let basic_block = ctx.context().append_basic_block(function, "entry");
        ctx.builder().position_at_end(basic_block);

        // Record the function parameters in the NamedValues map.
        ctx.named_values_mut().clear();
        for (i, arg) in function.get_param_iter().enumerate() {
            let arg = arg.into_float_value();

            // Create aliases for arguments.
            // let name = arg.get_name().to_str().unwrap();
            let name = self.prototype.args[i].as_str();

            ctx.named_values_mut()
                .insert(name.to_string(), arg.as_any_value_enum());
        }

        match self.body.code_gen(ctx) {
            Ok(value) => {
                let value = value.into_float_value();

                // Finish off the function.
                ctx.builder().build_return(Some(&value));

                // Validate the generated code, checking for consistency.
                function.verify(true);

                Ok(AnyValueEnum::FunctionValue(function))
            }
            Err(e) => {
                // Error reading body, remove function.
                // If we didn’t delete it, it would live in the symbol table, with a body, preventing future redefinition.
                // SAFETY: LLVM function is not used after this point.
                unsafe { function.delete() };

                Err(e)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number() {
        let ast = ExprAst::Number(1.0);
        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_float_value().print_to_string().to_string(),
            "double 1.000000e+00"
        );
    }

    #[test]
    fn test_variable() {
        let ast = ExprAst::Variable("x".to_string());
        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        ctx.named_values_mut().insert(
            "x".to_string(),
            AnyValueEnum::FloatValue(context.f64_type().const_float(1.0)),
        );
        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_float_value().print_to_string().to_string(),
            "double 1.000000e+00"
        );
    }

    #[test]
    fn test_binary_add() {
        let ast = ExprAst::Binary {
            op: "+".to_string(),
            lhs: Box::new(ExprAst::Number(1.0)),
            rhs: Box::new(ExprAst::Number(2.0)),
        };
        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_float_value().print_to_string().to_string(),
            "double 3.000000e+00"
        );
    }

    #[test]
    fn test_binary_lt() {
        let ast = ExprAst::Binary {
            op: "<".to_string(),
            lhs: Box::new(ExprAst::Number(0.0)),
            rhs: Box::new(ExprAst::Number(2.0)),
        };
        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_float_value().print_to_string().to_string(),
            "double 1.000000e+00"
        );
    }

    #[test]
    fn test_if_true() {
        // function
        let function = FunctionAst {
            prototype: PrototypeAst {
                name: "foo".to_string(),
                args: vec![],
            },
            body: ExprAst::If {
                cond: Box::new(ExprAst::Number(0.2)),
                then: Box::new(ExprAst::Number(2.0)),
                else_: Box::new(ExprAst::Number(3.0)),
            },
        };

        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = function.code_gen(&mut ctx).unwrap();

        let expected = "define double @foo() {
entry:
  br i1 true, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi double [ 2.000000e+00, %then ], [ 3.000000e+00, %else ]
  ret double %iftmp
}
";

        assert_eq!(
            value.into_function_value().print_to_string().to_string(),
            expected
        );
    }

    #[test]
    fn test_if_false() {
        // function
        let function = FunctionAst {
            prototype: PrototypeAst {
                name: "foo".to_string(),
                args: vec![],
            },
            body: ExprAst::If {
                cond: Box::new(ExprAst::Number(0.0)),
                then: Box::new(ExprAst::Number(2.0)),
                else_: Box::new(ExprAst::Number(3.0)),
            },
        };

        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = function.code_gen(&mut ctx).unwrap();

        let expected = "define double @foo() {
entry:
  br i1 false, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi double [ 2.000000e+00, %then ], [ 3.000000e+00, %else ]
  ret double %iftmp
}
";

        assert_eq!(
            value.into_function_value().print_to_string().to_string(),
            expected
        );
    }

    #[test]
    fn test_if_something_in_blocks() {
        let foo = PrototypeAst {
            name: "foo".to_string(),
            args: vec![],
        };
        let bar = PrototypeAst {
            name: "bar".to_string(),
            args: vec![],
        };

        let baz = FunctionAst {
            prototype: PrototypeAst {
                name: "baz".to_string(),
                args: vec!["x".to_string()],
            },
            body: ExprAst::If {
                cond: Box::new(ExprAst::Variable("x".to_string())),
                then: Box::new(ExprAst::Call {
                    callee: "foo".to_string(),
                    args: vec![],
                }),
                else_: Box::new(ExprAst::Call {
                    callee: "bar".to_string(),
                    args: vec![],
                }),
            },
        };

        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);

        foo.code_gen(&mut ctx).unwrap();
        bar.code_gen(&mut ctx).unwrap();
        let value = baz.code_gen(&mut ctx).unwrap();

        assert_eq!(
            value.into_function_value().print_to_string().to_string(),
            "define double @baz(double %0) {
entry:
  %ifcond = fcmp one double %0, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %calltmp = call double @foo()
  br label %ifcont

else:                                             ; preds = %entry
  %calltmp1 = call double @bar()
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi double [ %calltmp, %then ], [ %calltmp1, %else ]
  ret double %iftmp
}
",
        );
    }

    #[test]
    fn test_prototype() {
        let ast = PrototypeAst {
            name: "foo".to_string(),
            args: vec!["x".to_string(), "y".to_string()],
        };
        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_function_value().print_to_string().to_string(),
            "declare double @foo(double, double)\n"
        );
    }

    #[test]
    fn test_function() {
        let ast = FunctionAst {
            prototype: PrototypeAst {
                name: "foo".to_string(),
                args: vec!["x".to_string(), "y".to_string()],
            },
            body: ExprAst::Number(1.0),
        };
        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);
        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_function_value().print_to_string().to_string(),
            "define double @foo(double %0, double %1) {\nentry:\n  ret double 1.000000e+00\n}\n"
        );
    }

    #[test]
    fn test_call() {
        let function = FunctionAst {
            prototype: PrototypeAst {
                name: "foo".to_string(),
                args: vec!["x".to_string(), "y".to_string()],
            },
            body: ExprAst::Number(4.0),
        };

        let ast = ExprAst::Call {
            callee: "foo".to_string(),
            args: vec![ExprAst::Number(1.0), ExprAst::Number(2.0)],
        };

        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);

        function.code_gen(&mut ctx).unwrap();

        let value = ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_float_value().print_to_string().to_string(),
            "  %calltmp = call double @foo(double 1.000000e+00, double 2.000000e+00)"
        );
    }

    #[test]
    fn test_function_with_different_arg_name() {
        let extern_ast = PrototypeAst {
            name: "foo".to_string(),
            args: vec!["a".to_string()],
        };
        let function_ast = FunctionAst {
            prototype: PrototypeAst {
                name: "foo".to_string(),
                args: vec!["b".to_string()],
            },
            body: ExprAst::Variable("b".to_string()),
        };

        let context = inkwell::context::Context::create();
        let mut ctx = ModuleCtx::new("test", &context);

        extern_ast.code_gen(&mut ctx).unwrap();
        let value = function_ast.code_gen(&mut ctx).unwrap();
        assert_eq!(
            value.into_function_value().print_to_string().to_string(),
            "define double @foo(double %0) {\nentry:\n  ret double %0\n}\n"
        );
    }
}
