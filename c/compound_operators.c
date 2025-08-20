int main(void) {
  int a = 250;
  int b = 200;
  int c = 100;
  int d = 75;
  int e = 50;
  int f = 25;
  int g = 10;
  int h = 1;
  int j = 0;
  int x = 0;
  x = a &= b *= c |= d = e ^= f += g >>= h <<= j = 1;
  if (a != 40) {
    return 1;
  }
  if (b != 21800) {
    return 2;
  }
  if (c != 109) {
    return 3;
  }
  if (d != 41) {
    return 4;
  }
  if (e != 41) {
    return 5;
  }
  if (f != 27) {
    return 6;
  }
  if (g != 2) {
    return 7;
  }
  if (h != 2) {
    return 8;
  }
  if (j != 1) {
    return 9;
  }
  if (x != 40) {
    return 24;
  }
}
