int putchar(int ch);
void *malloc(unsigned long size);

int main(void) {
    struct s {
        char x;
        char y;
        char z;
    } b = {'a','b','c'};

    putchar(b.x);
    putchar('\n');

    struct s *g_ptr = &b;  // first, point to global struct from previous test
    g_ptr->x = g_ptr->x + 1;
    putchar(b.x);
    g_ptr->x = g_ptr->x + 1;
    putchar(b.x);
    // now declare a new struct and point to that instead
    g_ptr = malloc(sizeof(struct s));
    g_ptr->x = 'a';
    g_ptr->y = 'b';
    g_ptr->z = 'c';
    g_ptr->x = g_ptr->x + 1;
    putchar(b.x);
    g_ptr->x = g_ptr->x + 1;
    putchar(b.x);
}
