# C Compiler

C Compiler implementation in Rust based on the book [Writing a C Compiler by Nora Sandler](http://nostarch.com/writing-c-compiler)

Unit tests for testing compliance with the book's implementation can be found [here](https://github.com/nlsandler/writing-a-c-compiler-tests/). These tests proved to be incredibly helpful in narrowing down the root causes of many bugs that came up as I was implementing this compiler.

This compiler also implements several additional features not covered by the book, with the aim of supporting much of glibc's standard header files () with minimal manual intervention. `stddef.h` and `stdarg.h` needed to be reimplemented as glibc does not provide a default implementation for these (gcc and clang supply their own versions of these files). These features include:

- Typedef
- Enums
- Variadic functions including implementing `va_list`, `va_start` and `va_arg`.
- Reimplementing the whole C preprocessor (eg. handling `\#include` and `\#define` etc.)
- Some amount of constant-folding for static initialisers and enum values
- (Partial) Support for const, restrict, inline, volatile keywords
- Float, long double, long long int, short int
- Function declarations with anonymous parameters
- Structs and unions with anonymous fields

Test C files for these features can be found in the `c` directory, and this compiler's versions of `stddef.h` and `stdarg.h` are in the `include` directory.

## Running this application

Build:
```sh
cargo build --release
```

Running:
```sh
#after building
target/release/c-compiler [args] filename.c [filename2.c]

#otherwise
cargo run -- [args] filename.c [filename2.c]
```

Compiled C files are outputted to the same directory as the C file.

### Optional Compiler Args

- `--optimize`: (Recommended). Enable BIRDS optimizations such as constant folding and dead code elimination.
- `--preprocess`: Only preprocess the C file (resolve header files and strip comments).
- `--parse`: Only parse the C file, and emit the parsed result as Golang-esque pseudocode.
- `--validate`: Same as above, but preform type checking and annotate every variable with its type in the output.
- `--birds`: Convert this code to my IR implementation I've named Bodacious Intermediate Representation Design Spec (the book calls it TACKY).
- `-S`: Output the resulting assembly code to the terminal and also output the assembly code to `filename.s`.
- `some-filename.s`: Add an additional assembly file to pass to the linker along with this file.
- `-l[some-library]`: This arg is passed to the linker to link additional required libraries.
- `-c`: Instruct the linker to output an object (`.o`) file instead of an executable.
