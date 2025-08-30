#include <stdarg.h>

int puts(const char *c);

int sum(int n, ...) {
  va_list ap;
  va_start(ap, n); // initialize
  int total = 0;

  for (int i = 0; i < n; i++) {
    int x = va_arg(ap, int); // get next int argument
    total += x;
  }

  va_end(ap); // clean up
  return total;
}

int main(void) {
  int s = sum(3, 10, 20, 30);
  return s;
}
