int add(int a, int b) {
  return a + b;
}

int main(void) {
  int (*my_func)(int, int) = add;
  int c = my_func(5,6);

  return c;
}

