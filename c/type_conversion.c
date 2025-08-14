long global_one = 1l;
int main(void) {
    int num = 0;
    int *ptr = &num;
    *ptr = -100;

    unsigned long a = 3458764513821589504ul;  // convert double to ulong
    unsigned long arr2[4] = {
        3458764513821589504.0,  // convert double to ulong
        *ptr,
        (unsigned int)18446744073709551615UL,  // this is ULONG_MAX - truncate
        -global_one                            // converts to ULONG_MAX
    };

    unsigned long arr[4] = {
        3458764513821589504ul,  // convert double to ulong
        *ptr,  // dereference to get int, then convert to ulong - end up with
               // 2^64 - 100
        (unsigned int)18446744073709551615UL,  // this is ULONG_MAX - truncate
                                               // to unsigned int, then back to
                                               // ulong, end up with UINT_MAX
        -global_one                            // converts to ULONG_MAX
    };


    if (a != 3458764513821589504ul) {
        return 5;
    }
    if (arr2[0] != 3458764513821589504ul) {
        return 6;
    }
    if (!(arr[0] == 3458764513821589504ul))
        return 1;
    if (!(arr[1] == 18446744073709551516ul))
         return 2;
    if (!(arr[2] == 4294967295U))
         return 3;
    if (!(arr[3] == 18446744073709551615UL))
        return 4;
}
