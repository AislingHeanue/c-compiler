#include <time.h>
#include <string.h>

int puts(const char *c);

static void check(const char *name, int ok) {
    if (ok) {
        puts("PASS: "); puts(name);
    } else {
        puts("FAIL: "); puts(name);
    }
}

static time_t abs_time_diff(time_t a, time_t b) {
    if (a >= b) return a - b;
    return b - a;
}

int main(void) {
    // ---------- time() ----------
    time_t t1 = time(NULL);
    check("time() non-negative", t1 >= 0);

    time_t t2 = time(NULL);
    double delta = difftime(t2, t1);
    check("difftime() non-negative", delta >= 0);

    // ---------- ctime() ----------
    char *ct = ctime(&t1);
    check("ctime() not NULL", ct != NULL);
    if (ct) {
        check("ctime() ends with newline", ct[strlen(ct)-1] == '\n');
    }

    // ---------- localtime() ----------
    struct tm *lt = localtime(&t1);
    check("localtime() not NULL", lt != NULL);
    if (lt) {
        check("localtime() month range", lt->tm_mon >= 0 && lt->tm_mon <= 11);
        check("localtime() day range", lt->tm_mday >= 1 && lt->tm_mday <= 31);
        check("localtime() year >= 2025", lt->tm_year + 1900 >= 2025);
    }

    // ---------- gmtime() ----------
    struct tm *gt = gmtime(&t1);
    check("gmtime() not NULL", gt != NULL);
    if (gt) {
        check("gmtime() year >= 2025", gt->tm_year + 1900 >= 2025);
    }

    // ---------- mktime() ----------
    if (lt) {
        time_t roundtrip = mktime(lt);
        check("mktime(localtime) roundtrip",
              abs_time_diff(roundtrip, t1) < 86400);
    }

    return 0;
}
