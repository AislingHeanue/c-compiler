// https://gcc.gnu.org/onlinedocs/cpp.pdf section 3.10.6: argument prescan
#define f(x) x + 1

#define AFTERX(x) X_ ## x
#define XAFTERX(x) AFTERX(x)
#define TABLESIZE 1024
#define BUFSIZE TABLESIZE
#define STR(x) #x
#define XSTR(x) STR(x)

// unshielded commmas here can break functional macros that expand this (expected)
#define foo a,b
#define foo_fixed (a,b)
#define bar(x) lose(x)
#define lose(x) (1 + (x))

#define ignore_second_arg(a,b,c) a; c


int puts(char *c);

int main(void) {
    puts("should be 20 + 1 + 1:");
    puts(XSTR(f(f(20))));
    puts("should be X_BUFSIZE:");
    puts(XSTR(AFTERX(BUFSIZE)));
    puts("should be X_1024:");
    puts(XSTR(XAFTERX(BUFSIZE)));
    puts("should be (1 + ((a,b))):");
    puts(XSTR(bar(foo_fixed)));

    ignore_second_arg(1,
                      ignored (),
                      2);
}
