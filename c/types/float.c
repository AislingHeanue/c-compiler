static float s = 0.02f;

int main(void) {
  float a = 1.0F;
  float b = .2f + a;
  float c = 3.F * b;
  float d[5] = {2,2l,2ul,2u,'a'};

  int e = c + d[4];
  short f = e + 3./s;

  short g = d[0]/d[1];

  int h = 1.0f;
  unsigned long i = 1.0f;
  double j = 1.0f;

  float k = 1.0;
  float l = 1;
  float m = 1ul;

  return f;
}
