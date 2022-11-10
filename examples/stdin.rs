extern crate futures;

use std::io;
use std::thread;

use futures::channel::mpsc::channel;
use futures::{SinkExt, Stream};
use inkwell::values::AnyValue;
use kaleidoscope::ast::AnyAst;
use kaleidoscope::code_gen_ctx::CodeGenCtx;
use kaleidoscope::code_gen_visitor::CodeGenVisitor;
use kaleidoscope::parser::Parser;

fn main() {
    let mut stdin = stdin();

    let mut parser = Parser::from_char_stream(&mut stdin);

    let context = inkwell::context::Context::create();
    let mut ctx = CodeGenCtx::new("test", &context);

    let task = async {
        loop {
            // Parse an AST from the input stream.
            let (any_ast, is_top_level) = match parser.parse_any().await {
                Ok(any_ast) => match any_ast {
                    Some(any_ast) => {
                        let is_top_level = match &any_ast {
                            AnyAst::Function(function) => function.prototype.name == "",
                            _ => false,
                        };
                        (any_ast, is_top_level)
                    }
                    None => break, // EOF
                },
                Err(e) => {
                    eprintln!("Error(parser): {:?}", e);
                    match parser.skip_until_semicolon().await {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Error(parser, fatal): {:?}", e);
                            break;
                        }
                    };
                    continue;
                }
            };

            // Code generation.
            let value = match any_ast.code_gen(&mut ctx) {
                Ok(value) => value,
                Err(e) => {
                    eprintln!("Error(codegen): {:?}", e);
                    continue;
                }
            };

            // Print the generated code.
            println!("; {}", value.print_to_string().to_string());

            // Run the generated code.
            // JIT
            if is_top_level {
                let top_level_func = value.into_function_value();

                // SAFETY: The execution engine takes ownership of the module.
                let execution_engine = match ctx.module().create_execution_engine() {
                    Ok(execution_engine) => execution_engine,
                    Err(e) => {
                        eprintln!("Error(execution_engine): {:?}", e);
                        continue;
                    }
                };

                // SAFETY: The generated code cannot mess with the heap memory yet.
                let value = unsafe { execution_engine.run_function(top_level_func, &[]) };

                // Print the result.
                println!(
                    "; Evaluated to {}",
                    value.as_float(&ctx.context().f64_type()).to_string()
                );

                // Clear the top-level function.
                // SAFETY: The function is not used anymore.
                unsafe { top_level_func.delete() };

                // Clear the execution engine.
                // SAFETY: The execution engine release the module.
                let res = execution_engine.remove_module(ctx.module());
                match res {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Error(execution_engine, fatal): {:?}", e);
                        break;
                    }
                };
            }
        }
    };

    futures::executor::block_on(task);
}

fn stdin() -> impl Stream<Item = char> {
    let (mut tx, rx) = channel(1);
    thread::spawn(move || {
        let task = async {
            let input = io::stdin();

            let lines = input.lines();

            for res in lines {
                match res {
                    Ok(line) => {
                        for c in line.chars() {
                            match tx.feed(c).await {
                                Ok(_) => (),
                                Err(e) => {
                                    println!("Error(stdin): {}", e);
                                    break;
                                }
                            };
                        }
                        match tx.feed('\n').await {
                            Ok(_) => (),
                            Err(e) => {
                                println!("Error(stdin): {}", e);
                                break;
                            }
                        };
                        match tx.flush().await {
                            Ok(_) => (),
                            Err(e) => {
                                println!("Error(stdin): {}", e);
                                break;
                            }
                        };
                    }
                    Err(e) => {
                        eprintln!("error(stdin): {}", e);
                        break;
                    }
                }
            }
        };

        futures::executor::block_on(task);
    });

    rx
}
