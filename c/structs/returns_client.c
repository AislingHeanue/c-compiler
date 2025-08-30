/* Test that we return a wide range of struct types according to the ABI */
#include "returns.h"

struct memory same_file_return_on_stack(void) {
    struct memory retval = {1.25, "xy", 100l, 44};
    return retval;
}

int main(void) {

    struct memory s7 = return_on_stack();

    return 0;
}
