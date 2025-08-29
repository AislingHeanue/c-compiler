int puts(const char *c);

int stage_result = 0;    // accumulates numeric results
int global_result = 0;   // final result

// Simple functions
int add1(int x) { return x + 1; }
int sub1(int x) { return x - 1; }
int zero(void) { return 0; }

// Function returning function pointer
int (*get_add1(void))(int) { return add1; }
int (*get_sub1(void))(int) { return sub1; }

// Typedef for function pointer
typedef int (*int_unary_fn)(int);

// Nested function pointer usage
int nested_test(int (*f)(int), int (*g)(int)) {
    int (*(*h)(void))(int) = get_add1;
    int (*ret)(int) = h();
    return f(g(ret(10)));  // f(g(ret(10)))
}

// Array of function pointers
int (*fp_array[2])(int) = { add1, sub1 };

// Function pointer as parameter
int apply(int (*fn)(int), int x) {
    return fn(x);
}

// Function pointer returning pointer-to-function
int (*foo(void))(void) { return zero; }

// Helper: check actual vs expected
void check(int actual, int expected, const char *stage) {
    puts(stage);
    if(actual == expected)
        puts("PASS");
    else
        puts("FAIL");
}

int test_main(void) {
    int result;

    // Stage 1: Direct call via function pointer
    int (*fp)(int) = add1;
    result = fp(5);    // 6
    check(result, 6, "Stage 1: Direct call");

    // Stage 2: Explicit &function
    fp = &sub1;
    result = fp(5);    // 4
    check(result, 4, "Stage 2: &function assignment");

    // Stage 3: Typedef pointer
    int_unary_fn ftyp = add1;
    result = ftyp(10); // 11
    check(result, 11, "Stage 3: Typedef pointer");

    // Stage 4: Nested function pointer
    result = nested_test(add1, sub1); // 11
    check(result, 11, "Stage 4: Nested function pointer");

    // Stage 5: Array of function pointers
    result = fp_array[0](1) ;  // add1(1) = 2
    check(result, 2, "Stage 5a: Array element 0");
    result = fp_array[1](2) ;  // sub1(2) = 1
    check(result, 1, "Stage 5b: Array element 1");

    // Stage 6: Function returning function pointer
    int (*ret)(int) = get_add1();
    result = ret(7);           // 8
    check(result, 8, "Stage 6: Function returning function pointer");

    // Stage 7: Function pointer returning pointer-to-function
    int (*(*fnp)(void))(void) = foo;
    result = fnp()();          // zero() = 0
    check(result, 0, "Stage 7: Function returning pointer-to-function");

    puts("All tests complete");

    global_result = 0;  // could sum actual results if desired
    return 0;
}

int main(void) {
    test_main();
    return global_result;
}
