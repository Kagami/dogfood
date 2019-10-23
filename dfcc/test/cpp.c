#include "test.h"

#define A 1

  /* test */ #define B 2

/**
 * test
 */ #define C 3

    # /* test */ define D 4

#define MUL (2*3)

void test() {
  expect(1, A, "");
  expect(2, B, "");
  expect(3, C, "");
  expect(4, D, "");
  expect(6, MUL, "");
}
