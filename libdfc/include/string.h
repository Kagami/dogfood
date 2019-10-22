#define NULL 0

size_t strlen(const char *s);
int strcmp(const char *s1, const char *s2);
int strncmp(const char *s1, const char *s2, size_t n);
char *strstr(const char *haystack, const char *needle);

char *strerror(int errnum);

void *memcpy(void *dst, const void *src, size_t n);
int memcmp(const void *s1, const void *s2, size_t n);
void *memchr(const void *s, int c, size_t n);

#if _POSIX_C_SOURCE >= 200809L
char *strndup(const char *s, size_t n);
#endif
