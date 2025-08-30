int puts(const char *s);

enum Color { RED, GREEN = 5, BLUE };  // RED=0, GREEN=5, BLUE=6
enum Flags { F1=1, F2=2, F4=4 };

struct TestStruct {
    enum { A, B, C } x;  // anonymous enum inside struct
};

int main(void) {
    int failed = 0;

    // Test 1: Basic enum values
    if (RED == 0 && GREEN == 5 && BLUE == 6)
        puts("PASS");
    else {
        puts("FAIL");
        failed++;
    }

    // Test 2: Enum as bit flags
    int flags = F1 | F4;  // should be 1|4 = 5
    if (flags == 5)
        puts("PASS");
    else {
        puts("FAIL");
        failed++;
    }

    // Test 3: Anonymous enum in struct
    struct TestStruct t;
    t.x = B;  // B == 1
    if (t.x == 1)
        puts("PASS");
    else {
        puts("FAIL");
        failed++;
    }

    // Test 4: Assigning values outside enumerators (legal in C)
    enum Color c;
    c = 42;
    if (c == 42)
        puts("PASS");
    else {
        puts("FAIL");
        failed++;
    }

    // Test 5: Using character literal in enum
    enum CharEnum { X = 'x', Y = 'y' };
    if (X == 'x' && Y == 'y')
        puts("PASS");
    else {
        puts("FAIL");
        failed++;
    }

    // Test 6: Inner block enum redeclaration
    {
        enum { D = 7, E = 8 } inner;
        inner = D;
        if (inner == 7)
            puts("PASS");
        else {
            puts("FAIL");
            failed++;
        }
    }

    // Test 7: Non-monotonic explicit enum values
    enum Weird { W1 = 2, W2 = 1, W3 }; // W3 should be 2
    if (W1 == 2 && W2 == 1 && W3 == 2)
        puts("PASS");
    else {
        puts("FAIL");
        failed++;
    }

    // Overall result
    if (failed == 0)
        puts("ALL TESTS PASSED");
    else
        puts("SOME TESTS FAILED");

    return 0;
}
