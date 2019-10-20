#include "test.h"

static const char *sglobal = "111" "222";

void test() {
  expect(0, strcmp(sglobal, "111222"), "");

  const char *slocal = "222" "333";
  expect(0, strcmp(slocal, "222333"), "");

  const char s[] = "123" "456";
  expect(7, sizeof(s));
  expect(0, strcmp(s,
    "12"
    "3456"
  ), "");
}
