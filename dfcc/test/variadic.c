// TODO(Kagami): Move to test.h
int assert_eq(long expected, long actual, char *code) {
  if (expected != actual) {
    printf("%s => %ld expected but got %ld\n", code, expected, actual);
    exit(1);
  }
}

// TODO(Kagami): Implement va_arg
/*int add_all1(int x, ...) {
  va_list ap;
  va_start(ap, x);

  for (;;) {
    int y = va_arg(ap, int);
    if (y == 0)
      return x;
    x += y;
  }
}*/

/*int add_all3(int x, int y, int z, ...) {
  va_list ap;
  va_start(ap, z);
  x = x + y + z;

  for (;;) {
    int y = va_arg(ap, int);
    if (y == 0)
      return x;
    x += y;
  }
}*/

int vsprintf_check(char *answer, char *fmt, ...) {
  char result[30] = { 0 };
  va_list ap;
  va_start(ap, fmt);
  vsprintf(result, fmt, ap);
  return strcmp(answer, result);
}

int main() {
  // assert_eq(6, add_all1(1,2,3,0), "add_all1(1,2,3,0)");
  // assert_eq(5, add_all1(1,2,3,-1,0), "add_all1(1,2,3,-1,0)");

  // assert_eq(6, add_all3(1,2,3,0), "add_all3(1,2,3,0)");
  // assert_eq(5, add_all3(1,2,3,-1,0), "add_all3(1,2,3,-1,0)");

  assert_eq(0, vsprintf_check("123 456", "123 %d", 456));

  printf("OK\n");
  return 0;
}
