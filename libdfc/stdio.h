typedef struct FILE FILE;
extern FILE *stdout;
extern FILE *stderr;

void *malloc(long size);
void *calloc(long nmemb, long size);
int *__errno_location();
char *strerror(int errnum);
FILE *fopen(char *pathname, char *mode);
long fread(void *ptr, long size, long nmemb, FILE *stream);
int feof(FILE *stream);
static void assert() {}
int strcmp(char *s1, char *s2);
int printf(char *fmt, ...);
int sprintf(char *buf, char *fmt, ...);
long strlen(char *p);
int strncmp(char *p, char *q);
void *memcpy(char *dst, char *src, long n);
char *strndup(char *p, long n);
int isspace(int c);
char *strstr(char *haystack, char *needle);
long strtol(char *nptr, char **endptr, int base);

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
