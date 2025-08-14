int strcmp(char *s1, char *s2);
int putchar(int arg);

struct large {
    int i;
    double d;
    char arr[10];

};
int test_large(struct large s) {
    if (s.i != 200000)
        return 1;
    if (s.d != 23.25)
        return 2;
    if (strcmp(s.arr, "abcdefghi"))
        return 3;

    return 0;  // success
}


int main(void) {
    struct large s4 = {200000, 23.25, "abcdefghi"};

    int a2 = test_large(s4);
    if (a2) return a2 + 10;

}
