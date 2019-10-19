// TODO(Kagami): Implement va_arg.
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

void test() {
  // expect(6, add_all1(1,2,3,0), "add_all1(1,2,3,0)");
  // expect(5, add_all1(1,2,3,-1,0), "add_all1(1,2,3,-1,0)");

  // expect(6, add_all3(1,2,3,0), "add_all3(1,2,3,0)");
  // expect(5, add_all3(1,2,3,-1,0), "add_all3(1,2,3,-1,0)");

  expect(0, vsprintf_check("123 456", "123 %d", 456));
}
