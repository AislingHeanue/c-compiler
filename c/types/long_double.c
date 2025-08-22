static long double s = 0.02l;

int main(void) {
  long double a = 1.0L;
  long double b = .2l + a;
  long double c = 3.l * b;
  long double d[5] = {2,2l,2ul,2u,'a'};

  int e = c + d[4];
  short f = e + 3.f/s;

  short g = d[0]/d[1];

  int h = 1.0l;
  unsigned long i = 1.0L;
  double j = 1.0l;

  long double k = 1.0;
  long double l = 1;
  long double m = 1ul;

  return f;
}
