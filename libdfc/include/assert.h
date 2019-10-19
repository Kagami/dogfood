void __assert_fail(const char *expr, const char *file, int line, const char *func);

// TODO(Kagami): Implement as macro, NDEBUG.
static void assert(long cond) {
  if (!cond) {
    __assert_fail(NULL, NULL, 0, NULL);
  }
}
