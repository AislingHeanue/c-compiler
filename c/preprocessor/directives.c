// preproc_test.c
// Preprocessor torture test for compilers (no printf, only puts)
// "GOOD" marks the expected/desired case, "BAD" marks an error branch.

// ----- Macro definitions -----
#define A 1
#define B 2
#define C (A + B)

// Redefinition with same value (legal)
#define D 4
#define D 4

// Redefinition with different value (should warn/error on strict compilers)
#define E 5
#define E 6

// Function-like macros
#define FOO(x) (x + 1)
#define BAR(x,y) x##y           // token pasting
#define STR(x) LITERAL_STR(x)   // stringification
#define LITERAL_STR(x) #x       // stringification but actually (need a level of indirections so macros get resolved)
#define EMPTY()

// Recursive-like macro (should stop expanding eventually)
#define SELF SELF

// Macro using another macro
#define DOUBLE(x) (2 * (x))
#define NESTED(x) DOUBLE(FOO(x))

// Empty-argument function-like macro
#define IDENTITY(x) x
#define NOTHING(x)

// ----- Conditional compilation -----
#if 0
const char* dead_code_str = "BAD: Entered #if 0 block (should not compile)";
#endif

#if 1
const char* compiled = "GOOD: #if 1 branch compiled";
#else
const char* compiled = "BAD: #if 1 branch skipped";
#endif

#ifdef A
const char* has_A_str = "GOOD: A is defined";
#else
const char* has_A_str = "BAD: A is not defined";
#endif

#ifndef Z
const char* no_Z_str = "GOOD: Z is not defined";
#else
const char* no_Z_str = "BAD: Z is defined";
#endif

// Arithmetic and undefined identifiers
#if 2 + 2 == 4
const char* math_ok_str = "GOOD: math check (2+2==4)";
#else
const char* math_ok_str = "BAD: math check failed";
#endif

#if defined(A) && !defined(Z)
const char* logic_ok_str = "GOOD: logic (A defined, Z not defined)";
#else
const char* logic_ok_str = "BAD: logic test failed";
#endif

#if UNDEFINED_MACRO
const char* oops_str = "BAD: Entered #if UNDEFINED_MACRO";
#else
const char* oops_str = "GOOD: Skipped #if UNDEFINED_MACRO (treated as 0)";
#endif

// Nested conditionals
#if 1
  #if 0
  const char* nested_str = "BAD: nested inner #if 0 taken";
  #else
  const char* nested_str = "GOOD: nested inner #else taken";
  #endif
#else
  const char* nested_str = "BAD: outer #if 1 skipped";
#endif

int puts(char *c);
// ----- Testing macros in code -----
int main(void) {
    // Expand macros and show as string literals via puts
    puts("Macro expansion check: x = " STR(A) " + " STR(B) " + " STR(C) " (should be 1+2+(1+2))");
    puts("Macro expansion check: FOO(10) = " STR(FOO(10)) " (expected (10 + 1))");
    puts("Macro expansion check: NESTED(3) = " STR(NESTED(3)) " (expected 2*(3+1))");

    // Token pasting
    int BAR(va, lue) = 42;
    if (value == 42)
        puts("GOOD: BAR(va,lue) produced variable 'value' = 42");
    else
        puts("BAD: BAR(va,lue) did not produce expected variable");

    // Stringification
    puts("Stringification: STR(hello) = " STR(hello) " (GOOD if shows 'hello')");

    // Empty macro expansion
    puts("Empty macro expansion: '123" STR(EMPTY()) "' (GOOD if just '123')");

    // Identity macro
    puts("Identity macro: IDENTITY(123) = " STR(IDENTITY(123)) " (GOOD if shows 123)");

    // Recursive macro
    puts("Recursive macro: SELF = " STR(SELF) " (GOOD if shows 'SELF')");

    // Conditional results
    puts(compiled);
    puts(has_A_str);
    puts(no_Z_str);
    puts(math_ok_str);
    puts(logic_ok_str);
    puts(oops_str);
    puts(nested_str);

    return 0;
}
