int getchar(void);
int puts(char *c);
char *strncat(char *s1, char *s2, unsigned long n);
char *strcat(char *s1, char *s2);
unsigned long strlen(char *s);

static char name[30];
static char message[35] = "Hi, ";

int main(void) {
    puts("Enter your name: ");
    int idx = 0;
    while (idx < 29) {
        int c = getchar();
        if (c <= 0 || c == '\n') {
            break;
        }
        name[idx++] = c;
    }
    name[idx] = 0; // null terminator

    // pad message by 2 characters to leave room for ! and null
    strncat(message, name, 35 - strlen(message) - 2);
    strcat(message, "!");
    puts(message);
}
