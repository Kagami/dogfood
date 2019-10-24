#include <stddef.h>

#define R_OK 4
int access(const char *pathname, int mode);

#if _POSIX_C_SOURCE >= 2
int getopt(int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;
#endif
