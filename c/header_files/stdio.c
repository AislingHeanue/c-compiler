#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    FILE *fp;
    char buf[256];
    char fname[L_tmpnam];

    printf("=== STDIO TEST BEGIN ===\n");

    /* Test tmpnam + fopen */
    if (tmpnam(fname) == NULL) {
        perror("tmpnam failed");
        return 1;
    }
    printf("Temporary filename: %s\n", fname);

    fp = fopen(fname, "w+");
    if (!fp) {
        perror("fopen failed");
        return 1;
    }

    /* Test fprintf / fputs */
    fprintf(fp, "Hello %s %d\n", "World", 42);
    fputs("Second line\n", fp);

    /* Test fseek + ftell */
    long pos = ftell(fp);
    printf("Position after writes: %ld\n", pos);

    fseek(fp, 0, SEEK_SET);

    /* Test fgets */
    if (fgets(buf, sizeof buf, fp)) {
        printf("Read line 1: %s", buf);
    }
    if (fgets(buf, sizeof buf, fp)) {
        printf("Read line 2: %s", buf);
    }

    /* Test rewind + fscanf */
    rewind(fp);
    char word[16];
    int num;
    if (fscanf(fp, "Hello %15s %d\n", word, &num) == 2) {
        printf("fscanf got: %s %d\n", word, num);
    }

    /* Test fwrite + fread */
    rewind(fp);
    const char *raw = "RAW_DATA";
    size_t written = fwrite(raw, 1, strlen(raw), fp);
    printf("fwrite wrote %zu bytes\n", written);

    rewind(fp);
    memset(buf, 0, sizeof buf);
    size_t read = fread(buf, 1, written, fp);
    printf("fread read %zu bytes: %s\n", read, buf);

    /* Test fgetc + ungetc + fputc */
    rewind(fp);
    int c = fgetc(fp);
    printf("First char via fgetc: %c\n", c);
    ungetc(c, fp);
    c = fgetc(fp);
    printf("After ungetc, got again: %c\n", c);
    fputc('\n', fp);

    /* Test fflush */
    fflush(fp);

    /* Test snprintf + sscanf */
    char out[64];
    snprintf(out, sizeof out, "Number=%d String=%s", 123, "abc");
    printf("snprintf produced: %s\n", out);
    int x; char s[16];
    if (sscanf(out, "Number=%d String=%15s", &x, s) == 2) {
        printf("sscanf parsed: %d %s\n", x, s);
    }

    /* Test setvbuf */
    char localbuf[BUFSIZ];
    setvbuf(fp, localbuf, _IOFBF, BUFSIZ);

    /* Test error handling */
    fclose(fp);
    fp = fopen("/nonexistent/file", "r");
    if (!fp) {
        perror("expected fopen error");
    }

    /* Clean up */
    remove(fname);

    /* Test stderr */
    fprintf(stderr, "This is an error message on stderr\n");

    printf("=== STDIO TEST END ===\n");
    return 0;
}
