int putchar(int ch);

int main(void) {
    struct s {
        char x;
        char y;
        char z;
    } b = {'a','b','c'};
    struct s* ptr = &b;

    char a = ptr->z + 5;
    ptr->x = 'g';

    putchar(b.x);

    return 0;
}
