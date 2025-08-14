/* Test that we return a wide range of struct types according to the ABI */
#include <returns.h>

// return on stack + pass other int params
// struct memory pass_and_return_regs(int i, double d, struct int_and_xmm strct,
//                                    int c, struct two_ints t_i, long l,
//                                    struct one_int_exactly o_i_e, int c2);
// struct one_int return_int_struct(void) {
//     struct one_int retval = {1, 2};
//     return retval;
// }
//
// struct twelve_bytes return_two_int_struct(void) {
//     struct twelve_bytes retval = {10, "12345678"};
//     return retval;
// }
//
// struct one_xmm return_double_struct(void) {
//     struct one_xmm retval = {100.625};
//     return retval;
// }
// struct two_xmm return_two_double_struct(void) {
//     struct two_xmm retval = {{8.8, 7.8}};
//     return retval;
// }
// struct xmm_and_int return_mixed(void) {
//     struct xmm_and_int retval = {{10.0}, "ab"};
//     return retval;
// }
// struct int_and_xmm return_mixed2(void) {
//     struct int_and_xmm retval = {127, 34e43};
//     return retval;
// }
struct memory return_on_stack(void) {
    struct memory retval = {1.25, "xy", 100l, 44};
    return retval;
}
