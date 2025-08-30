static volatile int foo;
static int foo2;

void bar(void) {
    foo = 0;

    while (foo != 255)
      ;
}

void bar2(void) {
    foo2 = 0;

    while (foo2 != 255)
      ;
}

int main(void) {
  return 1;
}
