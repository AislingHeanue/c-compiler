#include <stdio.h>
#include <math.h>

void other_function(int a) {
  printf("%f\n", cos(a));
}

int main() {
  printf("%f\n", 1.1);
  other_function(2);
  printf("%f\n", 1.2);
  other_function(3);
  printf("%f\n", 1.3);
  other_function(4);
  printf("%f\n", 1.4);
  other_function(5);
  printf("%f\n", 1.5);
  other_function(6);
  printf("%f\n", 1.6);
  other_function(7);
}

