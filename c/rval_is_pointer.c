int main(void) {
    unsigned int ui = 4294967295U; // 2^32 - 1
    ui = ui / 5000.0;
    // convert ui to double, perform operation, and convert back
    if (ui != 858993u) {
        return 3; // fail
    }
}
