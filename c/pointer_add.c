/* Test pointer addition and subtraction to specify array indices
 * (but not subtracting two pointers to get the distance between them)
 * */

/* Addition */

/* add negative index to pointer */
int test_add_negative_index(void) {
    unsigned unsigned_arr[12] = {0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 42};
    unsigned *end_ptr = unsigned_arr + 12;

    unsigned *ptr = end_ptr + -10;
    return *ptr == 2;
}

int main(void) {
    /* Addition */

    if (!test_add_negative_index()) {
        return 2;
    }

    return 0;
}
