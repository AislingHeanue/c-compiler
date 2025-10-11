#include <stdarg.h>
#include <stdio.h>

int puts(const char *c);

int sum(int n, ...) {
  va_list ap;
  va_start(ap, n); // initialize
  int total = 0;
  volatile char b = 1;
  volatile char c = 1;
  volatile char d = 1;

  for (int i = 0; i < n; i++) {
    int x = va_arg(ap, int); // get next int argument
    int y = va_arg(ap, double); // get next int argument
    total += x;
    printf("%d\n", total);
  }

  va_end(ap); // clean up
  return total + b + c + d;
}

int main(void) {
  int s = sum(3, 10, 5., 20, 6., 30, 8.);
  return s;
}
