#include "test.h"

void test() {
  const int a = 2;
  const unsigned long b = 2;
  expect(1, a == b, "a == b");

  char*const c = 'a';
  int d = 97;
  expect(1, c == d, "c == d");

  int e = 1;
  int*const f = (int*const)&e;
  expect(1, *f == 1, "*e == 1");
}
