#include <stddef.h>

void updatePtrs(size_t* ptrA, size_t* ptrB, size_t* val) {
  *ptrA += *val;
  *ptrB += *val;
}

void updatePtrsRestricted(size_t* restrict ptrA, size_t* restrict ptrB, size_t* restrict val) {
  *ptrA += *val;
  *ptrB += *val;
}

int main(void) {
  return 1;
}
