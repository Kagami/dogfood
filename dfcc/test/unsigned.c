void test() {
  unsigned int a = 1;
  unsigned b = 2;
  expect(1, a != b, "a != b");

  expect(1, sizeof(char), "sizeof(char)");
  expect(1, sizeof(signed char), "sizeof(signed char)");
  expect(1, sizeof(signed char signed), "sizeof(signed char signed)");

  expect(2, sizeof(short), "sizeof(short)");
  expect(2, sizeof(int short), "sizeof(int short)");
  expect(2, sizeof(short int), "sizeof(short int)");
  expect(2, sizeof(signed short), "sizeof(signed short)");
  expect(2, sizeof(int short signed), "sizeof(int short signed)");

  expect(4, sizeof(int), "sizeof(int)");
  expect(4, sizeof(signed int), "sizeof(signed int)");
  expect(4, sizeof(signed), "sizeof(signed)");
  expect(4, sizeof(signed signed), "sizeof(signed signed)");

  expect(8, sizeof(long), "sizeof(long)");
  expect(8, sizeof(signed long), "sizeof(signed long)");
  expect(8, sizeof(signed long int), "sizeof(signed long int)");

  expect(8, sizeof(long long), "sizeof(long long)");
  expect(8, sizeof(signed long long), "sizeof(signed long long)");
  expect(8, sizeof(signed long long int), "sizeof(signed long long int)");

  expect(4, sizeof(0), "sizeof(0)");
  expect(8, sizeof(0UL), "sizeof(0UL)");
  expect(8, sizeof(0ULL), "sizeof(0ULL)");
  expect(8, sizeof(0lu), "sizeof(0lu)");
  expect(8, sizeof(0llu), "sizeof(0llu)");
  expect(8, sizeof(0x0UL), "sizeof(0x0UL)");
  expect(8, sizeof(0b0LU), "sizeof(0b0LU)");
}
