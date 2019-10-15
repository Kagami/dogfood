typedef struct FILE FILE;
extern FILE *stdout;
extern FILE *stderr;
FILE *fopen(char *pathname, char *mode);
long fread(void *ptr, long size, long nmemb, FILE *stream);
int feof(FILE *stream);

int printf(char *format, ...);
int fprintf(FILE *stream, char *format, ...);
int sprintf(char *str, char *format, ...);
int vprintf(char *format, va_list ap);
int vfprintf(FILE *stream, char *format, va_list ap);
int vsprintf(char *str, char *format, va_list ap);
