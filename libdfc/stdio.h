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

static void assert() {}
int *__errno_location();
void exit(int status);

void *malloc(long size);
void *calloc(long nmemb, long size);
void *memcpy(char *dst, char *src, long n);

typedef struct FILE FILE;
extern FILE *stdout;
extern FILE *stderr;
FILE *fopen(char *pathname, char *mode);
long fread(void *ptr, long size, long nmemb, FILE *stream);
int feof(FILE *stream);

int printf(char *fmt, ...);
int sprintf(char *buf, char *fmt, ...);
int fprintf(FILE *stream, char *fmt, ...);
int vfprintf(FILE *stream, char *fmt, va_list ap);

int isspace(int c);
int ispunct(int c);
int isdigit(int c);
long strlen(char *p);
int strcmp(char *s1, char *s2);
int strncmp(char *p, char *q);
int strncasecmp(char *s1, char *s2, long n);
char *strstr(char *haystack, char *needle);
long strtol(char *nptr, char **endptr, int base);
char *strndup(char *p, long n);
char *strerror(int errnum);
