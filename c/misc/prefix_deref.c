
int main(void) {
    int x = 10;
    int *y = &x;

    // Prefix ++
    if (++*y != 11) {
        return 1;
    }
    if (x != 11) {
        return 2;
    }
}
