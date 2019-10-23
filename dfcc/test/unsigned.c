#include "test.h"

void test() {
  unsigned int a = 1;
  unsigned b = 2;
  expect(1, a != b, "a != b");

  expect(1, sizeof(unsigned char), "sizeof(unsigned char)");
  expect(2, sizeof(unsigned short), "sizeof(unsigned short)");
  expect(2, sizeof(int short unsigned), "sizeof(int short unsigned)");
  expect(4, sizeof(unsigned int), "sizeof(unsigned int)");
  expect(4, sizeof(unsigned), "sizeof(unsigned)");
  expect(8, sizeof(unsigned long), "sizeof(unsigned long)");
  expect(8, sizeof(unsigned long int), "sizeof(unsigned long int)");
  expect(8, sizeof(unsigned long long), "sizeof(unsigned long long)");
  expect(8, sizeof(unsigned long long int), "sizeof(unsigned long long int)");

  expect(4, sizeof(0), "sizeof(0)");
  expect(8, sizeof(0UL), "sizeof(0UL)");
  expect(8, sizeof(0ULL), "sizeof(0ULL)");
  expect(8, sizeof(0lu), "sizeof(0lu)");
  expect(8, sizeof(0llu), "sizeof(0llu)");
  expect(8, sizeof(0x0UL), "sizeof(0x0UL)");
  expect(8, sizeof(0b0LU), "sizeof(0b0LU)");
}
