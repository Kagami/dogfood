#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "dfcc.h"

typedef struct {
  const char *inpath;
  const char *outpath;
  bool is_stdin;
  bool is_stdout;
} Opts;

static void usage(int rc) {
  fprintf(stderr,
    "usage:\n"
    /*"  dfcc [options] -o <outfile> <infile>\n"
    "  dfcc [-h | --help]\n"
    "\n"
    "options:\n"
    "  -h, --help    show this help message and exit\n"
    "  -o outfile    specify the output file\n"
    "  -c            compile the input file but do not link\n"
    "  -std=c11      comply to C11 language standard (default)\n"
    "  -Wall         enable all warnings (default)\n"
    "  -Wpedantic    enable strict ISO C compliance (default)\n"
    "  -Werror       treat warnings as errors\n"
    "  -fdump-ast    dump AST tree\n"
    "  -g            enable debug information\n"*/
  );
  exit(rc);
}

static const Opts *read_opts(int argc, char **argv) {
  if (argc != 2) usage(1);
  Opts *opts = calloc(1, sizeof(Opts));
  opts->inpath = argv[1];
  opts->outpath = NULL;
  opts->is_stdin = strcmp(opts->inpath, "-") == 0;
  opts->is_stdout = true;
  return opts;
}

static const char *read_file(const char *path, bool is_stdin) {
  FILE *fp;
  if (is_stdin) {
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    if (!fp) error("cannot open %s: %s", path, strerror(errno));
  }

  static const int filemax = 10 * 1024 * 1024;
  char *buf = malloc(filemax);
  int size = fread(buf, 1, filemax - 2, fp);
  if (!feof(fp)) error("%s: file too large", path);

  // Make sure that the string ends with "\n\0".
  if (size == 0 || buf[size - 1] != '\n') {
    buf[size++] = '\n';
  }
  buf[size] = '\0';
  return buf;
}

int main(int argc, char **argv) {
  // Parse opts.
  const Opts *opts = read_opts(argc, argv);

  // Prepare input.
  const char *indata = read_file(opts->inpath, opts->is_stdin);
  error_init(opts->inpath, indata);

  // Tokenize and parse.
  Token *token = lex(indata);
  Program *prog = parse(token);

  // Assign offsets to local variables.
  for (Function *fn = prog->fns; fn; fn = fn->next) {
    int offset = fn->has_varargs ? 56 : 0;
    for (VarList *vl = fn->locals; vl; vl = vl->next) {
      Var *var = vl->var;
      offset = align_to(offset, var->ty->align);
      offset += var->ty->size;
      var->offset = offset;
    }
    fn->stack_size = align_to(offset, 8);
  }

  // Traverse the AST to emit assembly.
  gen(prog);

  return 0;
}
