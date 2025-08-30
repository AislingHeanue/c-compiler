#include <stdlib.h>
#include <string.h>

int puts(const char *s);

static void check_int(const char *name, int expected, int actual) {
    if (expected == actual) {
        puts("PASS: "); puts(name);
    } else {
        puts("FAIL: "); puts(name);
    }
}

int main(void) {
    // ---------- abs / labs ----------
    check_int("abs(+5)", 5, abs(5));
    check_int("abs(-5)", 5, abs(-5));
    check_int("labs(-123456)", 123456, (int)labs(-123456));

    // ---------- atoi ----------
    check_int("atoi(\"42\")", 42, atoi("42"));
    check_int("atoi(\"   -99\")", -99, atoi("   -99"));
    check_int("atoi(\"0\")", 0, atoi("0"));

    // ---------- strtol ----------
    check_int("strtol(\"123\",10)", 123, (int)strtol("123", NULL, 10));
    check_int("strtol(\"7b\",16)", 123, (int)strtol("7b", NULL, 16));
    check_int("strtol(\"-100\",2)", -4, (int)strtol("-100", NULL, 2));

    // ---------- strtoul ----------
    check_int("strtoul(\"ff\",16)", 255, (int)strtoul("ff", NULL, 16));

    // ---------- rand / srand ----------
    srand(1);
    int r1 = rand();
    srand(1);
    int r2 = rand();
    check_int("rand() reproducible", r1, r2);

    // ---------- malloc / free ----------
    char *buf = (char*)malloc(4);
    if (buf) {
        buf[0] = 'O'; buf[1] = 'K'; buf[2] = 0;
        if (strcmp(buf,"OK") == 0) {
            puts("PASS: malloc/free");
        } else {
            puts("FAIL: malloc/free");
        }
        free(buf);
    } else {
        puts("FAIL: malloc returned NULL");
    }

    return 0;
}
