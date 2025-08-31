# Test C files

Many of these tests were copied or partially copied from [nlsandler/writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests) as part of development and verification of this compiler.

More extensive (though vibe-coded, so read at your own peril) tests are located in the header_files directory. These test the most commonly used glibc libraries. Note: `stdio.h` and any other header files that rely on `stdarg.h` and `stddef.h` located in the `include` directory. The compiler is set up to load header files from `./include`, so running it from the root of this repository eg. with `cargo run` should detect all the required files correctly.

Similar to gcc, using math.h requires passing `-lm` to the compiler.
