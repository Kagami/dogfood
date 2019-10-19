#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "dfcc.h"

typedef struct {
  const char *inpath;
  const char *outpath;
  bool is_stdin;
  bool is_stdout;
} Opts;

static void usage(const char *err_msg) {
  if (err_msg) {
    fprintf(stderr, "error: %s\n", err_msg);
  }
  fprintf(stderr, "usage: dfcc [-h] [options] -o <outfile> <infile>\n");
  if (!err_msg) {
    fprintf(stderr,
      "\n"
      /*"options:\n"
      "  -h            show this help message and exit\n"
      "  -o outfile    specify the output file\n"
      "  -c            compile the input file but do not link\n"
      "  -std=c11      comply to C11 language standard (default)\n"
      "  -Wall         enable all warnings (default)\n"
      "  -Wpedantic    enable strict ISO C compliance (default)\n"
      "  -Werror       treat warnings as errors\n"
      "  -fdump-ast    dump AST tree\n"
      "  -g            enable debug information\n"*/
    );
  }
  exit(err_msg ? 1 : 0);
}

static const Opts *read_opts(int argc, char **argv) {
  Opts *opts = calloc(1, sizeof(Opts));
  for (;;) {
    const int opt = getopt(argc, argv, ":o:W:f:hcg");
    if (opt == -1) break;
    switch (opt) {
    case 'o':
      opts->outpath = optarg;
      opts->is_stdout = strcmp(opts->outpath, "-") == 0;
      break;
    case 'W':
      break;
    case 'f':
      break;
    case 'h':
      usage(NULL);
    case 'c':
      break;
    case 'g':
      break;
    default:
      usage("invalid option");
    }
  }
  if (optind != argc - 1 || !opts->outpath) {
    // usage("output file required");
  }
  opts->inpath = argv[optind];
  opts->is_stdin = strcmp(opts->inpath, "-") == 0;
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
