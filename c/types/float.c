int putchar(int c);

static float s = 0.02f;

int main(void) {
  float a = 1.0F;
  putchar(97+a);
  float b = .2f + a;
  putchar(97+b);
  float c = 3.F * b;
  putchar(97+c);
  float d[5] = {2,2l,2ul,2u,'a'};
  putchar(d[4]);

  int e = c + d[4];
  putchar(e);
  short f = e + 3./s;
  putchar(f-150);

  short g = d[0]/d[1];

  int h = 1.0f;
  unsigned long i = 1.0f;
  double j = 1.0f;

  float k = 1.0;
  float l = 1;
  float m = 1ul;

  return f;
}
