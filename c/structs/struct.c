int main(void) {
    struct one {
        int field1;
        double field2;
        struct two {
            struct three {
                int field3;
            } nested;
        } nested;
        char field4;
    } x = {1, 1.0, {{1}}, 'a'};
}
