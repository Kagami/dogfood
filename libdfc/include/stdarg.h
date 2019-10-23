typedef struct {
  unsigned int gp_offset;
  unsigned int fp_offset;
  void *overflow_arg_area;
  void *reg_save_area;
} va_list[1];

#ifndef __NEED_va_list
void __builtin_va_start(va_list ap, void *last);
#define va_start __builtin_va_start
#define va_end
#endif
