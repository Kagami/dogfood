#if _POSIX_C_SOURCE >= 2
int getopt(int argc, char * const argv[], const char *optstring);

extern char *optarg;
extern int optind;
#endif
