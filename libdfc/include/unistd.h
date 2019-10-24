#include <stddef.h>
#include <sys/types.h>

#define R_OK 4
int access(const char *pathname, int mode);
int unlink(const char *pathname);

pid_t fork(void);
int execlp(const char *file, const char *arg, ...);

#if _POSIX_C_SOURCE >= 2
int getopt(int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;
#endif
