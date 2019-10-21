#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static const char *gColorBlack = "\33[30;1m";
static const char *gColorRed   = "\33[31;1m";
static const char *gColorReset = "\33[0m";

// TODO(Kagami): Infer "desc" via preprocessor.
static void expect(long expected, long actual, const char *desc) {
  if (expected != actual) {
    printf("# %sFAIL\n%s", gColorRed, gColorReset);
    printf("%s%s%s: %ld expected but got %ld\n", gColorBlack, desc, gColorReset, expected, actual);
    exit(1);
  }
}

void test();

int main() {
  test();
  printf(".");
  return 0;
}
