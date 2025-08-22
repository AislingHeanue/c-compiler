int main() {
  long sl = -6;
  unsigned long long ull = 5;

  // The result of this addition will be of type unsigned long long.
  // which means it will overflow to a value waaay larger than -1.
  // Expecting 1 to be returned here
  return sl+ull > 0ll;
}
