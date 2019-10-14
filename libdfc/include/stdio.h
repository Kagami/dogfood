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
