int main(void) {
    struct {
        int field1;
        double field2;
        struct {
            struct {
                int field3;
            };
        };
        char field4;
    } x = {1, 1.0, 1, 'a'};
}
