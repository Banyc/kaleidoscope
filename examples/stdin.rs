extern crate futures;

use std::io;
use std::thread;

use futures::channel::mpsc::channel;
use futures::{SinkExt, Stream};
use inkwell::values::AnyValue;
use kaleidoscope::ast::AnyAst;
use kaleidoscope::code_gen;
use kaleidoscope::code_gen::Visitor;
use kaleidoscope::jit;
use kaleidoscope::parser::Parser;

fn main() {
    let mut stdin = stdin();

    let mut parser = Parser::from_char_stream(&mut stdin);

    let context = inkwell::context::Context::create();
    let mut ctx = code_gen::ModuleCtx::new("test", &context);

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
                            eprintln!("Error(parser, recovery): {:?}", e);
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

                // SAFETY: The generated code cannot mess with the heap memory yet.
                let res = unsafe { jit::run_and_drop_func(&ctx.module(), top_level_func, &[]) };
                let value = match res {
                    Ok(value) => value,
                    Err(e) => match e {
                        jit::Error::CreateExecutionEngineError(e) => {
                            eprintln!("Error(jit): {:?}", e);
                            continue;
                        }
                        jit::Error::ReleaseModuleError(e) => {
                            eprintln!("Error(jit, fatal): {:?}", e);
                            break;
                        }
                        jit::Error::UndefFunctionsError(undef_functions) => {
                            eprintln!("Error(jit): Undefined functions: {:?}", undef_functions);
                            continue;
                        }
                    },
                };

                // Print the result.
                println!(
                    "; Evaluated to {}",
                    value.as_float(&ctx.context().f64_type()).to_string()
                );
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
