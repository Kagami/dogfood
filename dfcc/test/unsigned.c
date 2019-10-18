void test() {
  unsigned int a = 1;
  unsigned b = 2;
  assert_eq(1, a != b, "a != b");

  assert_eq(1, sizeof(char), "sizeof(char)");
  assert_eq(1, sizeof(signed char), "sizeof(signed char)");
  assert_eq(1, sizeof(signed char signed), "sizeof(signed char signed)");

  assert_eq(2, sizeof(short), "sizeof(short)");
  assert_eq(2, sizeof(int short), "sizeof(int short)");
  assert_eq(2, sizeof(short int), "sizeof(short int)");
  assert_eq(2, sizeof(signed short), "sizeof(signed short)");
  assert_eq(2, sizeof(int short signed), "sizeof(int short signed)");

  assert_eq(4, sizeof(int), "sizeof(int)");
  assert_eq(4, sizeof(signed int), "sizeof(signed int)");
  assert_eq(4, sizeof(signed), "sizeof(signed)");
  assert_eq(4, sizeof(signed signed), "sizeof(signed signed)");

  assert_eq(8, sizeof(long), "sizeof(long)");
  assert_eq(8, sizeof(signed long), "sizeof(signed long)");
  assert_eq(8, sizeof(signed long int), "sizeof(signed long int)");

  assert_eq(8, sizeof(long long), "sizeof(long long)");
  assert_eq(8, sizeof(signed long long), "sizeof(signed long long)");
  assert_eq(8, sizeof(signed long long int), "sizeof(signed long long int)");

  assert(4, sizeof(0), "sizeof(0)");
  assert(8, sizeof(0UL), "sizeof(0UL)");
  assert(8, sizeof(0ULL), "sizeof(0ULL)");
  assert(8, sizeof(0lu), "sizeof(0lu)");
  assert(8, sizeof(0llu), "sizeof(0llu)");
  assert(8, sizeof(0x0UL), "sizeof(0x0UL)");
  assert(8, sizeof(0b0LU), "sizeof(0b0LU)");
}
