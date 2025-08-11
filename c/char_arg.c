int puts(char *c);

int print_char(char c) {
    puts(&c);
    return 0;
}

int main(void) {
    return print_char('!');
}

