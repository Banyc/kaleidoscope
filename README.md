# Kaleidoscope

- e.g.

  ```py
  # Compute the x'th fibonacci number.
  def fib(x)
    if x < 3 then
      1
    else
      fib(x-1)+fib(x-2)

  # This expression will compute the 40th number.
  fib(40)
  ```

- e.g. import external functions

  ```py
  extern sin(arg);
  extern cos(arg);
  extern atan2(arg1 arg2);

  atan2(sin(.4), cos(42))
  ```

- constraints:
  - only datatype: double
  - no type declarations

## Dev environment

- llvm@14
- Set environment variable `LLVM_SYS_140_PREFIX` to the path of llvm@14
  - Also set it at `rust-analyzer.server.extraEnv` and `rust-analyzer.runnableEnv` in `settings.json` if you use vscode

## References

- <https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html>
