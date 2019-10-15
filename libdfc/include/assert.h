// TODO(Kagami): Implement as macro, NDEBUG
void __assert_fail(char *expr, char *file, int line, char *func);

static void assert(long cond) {
  if (!cond) {
    __assert_fail(NULL, NULL, 0, NULL);
  }
}
