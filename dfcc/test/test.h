static char gColorBlack[]   = { 27, '[', '3', '0', ';', '1', 'm', 0 };
static char gColorRed[] = { 27, '[', '3', '1', ';', '1', 'm', 0 };
static char gColorReset[]   = { 27, '[', '0', 'm', 0 };

// TODO(Kagami): Infer "desc" via preprocessor.
int assert_eq(long expected, long actual, char *desc) {
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
