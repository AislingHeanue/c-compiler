int putchar(int c);
static long double s = 0.02l;

int main(void) {
  long double a = 1.0L;
  putchar(97+a);
  long double b = .2l + a;
  putchar(97+b);
  long double c = 3.l * b;
  putchar(97+c);
  long double d[5] = {2,2l,2ul,2u,'a'};
  putchar(d[4]);

  int e = c + d[4];
  putchar(e);
  short f = e + 3.f/s;
  putchar(f-150);

  short g = d[0]/d[1];

  int h = 1.0l;
  unsigned long i = 1.0L;
  double j = 1.0l;

  long double k = 1.0;
  long double l = 1;
  long double m = 1ul;

  return f;
}
