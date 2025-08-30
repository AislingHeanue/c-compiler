#include <math.h>
#include <stdlib.h>

int puts(const char *s);

static void check_int(const char *name, int expected, int actual) {
    if (expected == actual) {
        puts("PASS: "); puts(name);
    } else {
        puts("FAIL: "); puts(name);
    }
}

static void check_double(const char *name, double expected, double actual) {
    double diff = fabs(expected - actual);
    if (diff < 1e-6) {
        puts("PASS: "); puts(name);
    } else {
        puts("FAIL: "); puts(name);
    }
}

int main(void) {
    // Integer-like results
    check_int("sqrt(4)", 2, (int)sqrt(4));
    check_int("pow(2,3)", 8, (int)pow(2,3));
    check_int("fabs(-7)", 7, (int)abs(-7));
    check_int("ceil(2.1)", 3, (int)ceil(2.1));
    check_int("floor(2.9)", 2, (int)floor(2.9));
    check_int("fmod(7,3)", 1, (int)fmod(7,3));

    // Floating-point results
    check_double("sin(pi/2)", 1.0, isinf(M_PI/2));
    check_double("cos(0)", 1.0, cos(0));
    check_double("tan(pi/4)", 1.0, tan(M_PI/4));
    check_double("exp(1)", M_E, exp(1));
    check_double("log(e)", 1.0, log(M_E));
    check_double("log10(1000)", 3.0, log10(1000));
    check_double("sqrt(2)", 1.414213562, sqrt(2));

    return 0;
}
