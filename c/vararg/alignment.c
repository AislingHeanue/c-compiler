#include <stdarg.h>

int puts(const char *c);

int sum(int n, ...) {
  va_list ap;
  va_start(ap, n); // initialize
  int a = 0;
  int b = 0;
  int c = 0;
  int d = 0;
  int e = 0;
  int f = 0;
  int g = 0;
  int h = 0;
  int i = 0;
  int j = 0;
  int k = 0;
  int l = 0;
  int m = 0;
  int o = 0;

  return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o;
}

int main(void) {
  int s = sum(3, 10, 20, 30);
  return s;
}
