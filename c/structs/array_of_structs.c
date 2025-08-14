 struct inner {
    long l;
    char arr[2];
};  // size: 8 bytes, alignment: 4 bytes

struct outer {
    char a;          // byte 0
    struct inner b;  // bytes 4-11

};  // size: 12 byte, alignm

// int validate_struct_array(struct outer *struct_array) {
//     return 1;  // success
// }
/* Test that we can pass a pointer to an array of structures as a parameter */

// static struct outer static_array[3] = {
//     {0, {0, {0, 0}}}, {2, {3, {4, 5}}}, {4, {6, {8, 10}}}};

int main(void) {
    struct outer struct_array[3] = {
        {0, {0, {0, 0}}}, {2, {3, {4, 5}}}, {4, {6, {8, 10}}}};

    // pass pointers to struct arrays with both static and automatic storage
    // both have same contents so we can validate them with the same function

    // if (!validate_struct_array(static_array)) {
    //     return 1;
    // }
    //
    // if (!validate_struct_array(auto_array)) {
    //     return 2;
    // }
    if (struct_array[1].a !=2)
        return struct_array[2].a;

    for (int i = 0; i < 3; i = i + 1) {
        if (struct_array[i].a != i * 2)
            return i*10 + 1;
        if (struct_array[i].b.l != i * 3)
            return i*10 + 2;
        if (struct_array[i].b.arr[0] != i * 4)
            return i*10 + 3;
        if (struct_array[i].b.arr[1] != i * 5)
            return i*10 + 4;
    }

    return 0;  // success
}
