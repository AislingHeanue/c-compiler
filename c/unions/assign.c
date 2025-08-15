int main(void) {
    struct {
        int a;
        union {
            double d;
            unsigned char e;
        };
    } var = {1, 0.1};

    return var.e;
}
