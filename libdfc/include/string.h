long strlen(char *p);
int strcmp(char *s1, char *s2);
int strncmp(char *s1, char *s2, long n);
char *strstr(char *haystack, char *needle);
char *strndup(char *p, long n);

char *strerror(int errnum);

void *memcpy(char *dst, char *src, long n);
int memcmp(void *s1, void *s2, long n);
void *memchr(void *s, int c, long n);
