#include <stdarg.h>
// #include <stdlib.h>
// #include <string.h>

int puts(const char *s);

// simple int -> string
static void itoa10(int x, char *buf) {
    char tmp[32];
    int i = 0;
    int neg = 0;

    if (x == 0) {
        buf[0] = '0'; buf[1] = 0;
        return;
    }
    if (x < 0) { neg = 1; x = -x; }

    while (x > 0) {
        tmp[i++] = '0' + (x % 10);
        x /= 10;
    }
    int j = 0;
    if (neg) buf[j++] = '-';
    while (i > 0) buf[j++] = tmp[--i];
    buf[j] = 0;
}

// very dumb double -> string (just prints int part)
static void dtoa_simple(double d, char *buf) {
    int x = (int)d;
    itoa10(x, buf);
}

void test_all(const char *label, int count, ...) {
    va_list ap;
    va_start(ap, count);

    puts(label);

    for (int i = 0; i < count; i++) {
        int tag = va_arg(ap, int);

        if (tag == 0) { // int
            int v = va_arg(ap, int);
            char buf[64];
            itoa10(v, buf);
            puts(buf);
        } else if (tag == 1) { // double
            double d = va_arg(ap, double);
            char buf[64];
            dtoa_simple(d, buf);
            puts(buf);
        } else if (tag == 2) { // string
            const char *s = va_arg(ap, const char*);
            puts(s);
        } else {
            puts("unknown tag");
        }
    }

    va_end(ap);
}

void test_at_least_6(int count, int a, int b, int c, int d, int e, int f, int g, ...) {
    va_list ap;
    va_start(ap, g);

    int res = a + b + c + d + e + f + g;
    char buf[64];
    itoa10(res, buf);
    puts(buf);

    for (int i = 7; i < count; i++) {
        int v = va_arg(ap, int);
        char buf[64];
        itoa10(v, buf);
        puts(buf);
    }
    va_end(ap);
}

int main(void) {
    // ints only
    test_all("ints regs", 6,
        0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6);

    // more ints -> stack
    test_all("ints stack", 8,
        0, 10, 0, 11, 0, 12, 0, 13, 0, 14, 0, 15, 0, 16, 0, 17);

    // floats only
    test_all("floats regs", 4,
        1, 1.1, 1, 2.2, 1, 3.3, 1, 4.4);

    // floats + stack
    test_all("floats stack", 10,
        1, 5.5, 1, 6.6, 1, 7.7, 1, 8.8, 1, 9.9,
        1, 10.1, 1, 11.1, 1, 12.1, 1, 13.1, 1, 14.1);

    // mixed
    test_all("mixed", 6,
        0, 42,
        1, 3.14159,
        2, "hello",
        0, -7,
        1, 2.71828,
        2, "world");

    test_at_least_6(8, 1, 2, 3, 4, 5, 6, 7, 28);

    return 0;
}
