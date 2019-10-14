typedef struct {
  int gp_offset;
  int fp_offset;
  void *overflow_arg_area;
  void *reg_save_area;
} __va_elem;

typedef __va_elem va_list[1];

static void va_start(__va_elem *ap) {
  __builtin_va_start(ap);
}

static void va_end(__va_elem *ap) {}
