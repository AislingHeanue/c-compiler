int strcmp(char *s1, char *s2);
void exit(int status);

struct stack_bytes {
    char bytes[16];
};

static struct stack_bytes to_validate;

// test case 1: return a struct in a general-purpose register
struct one_int_reg {
    char cs[7];
};

struct one_int_reg return_int_struct(void) {
    struct one_int_reg retval = {{0, 0, 0, 0, 0, 0, 0}};
    return retval;
}

static struct one_int_reg one_int_struct;

int main(void) {
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    one_int_struct = return_int_struct();
    to_validate = bytes;
    if (strcmp(to_validate.bytes, "efghijklmnopqrs")) {
        exit(1);
    }

    for (int i = 0; i < 7; i = i + 1) {
        if (one_int_struct.cs[i]) {
            exit(2);
        }
    }

    return 0;
}
