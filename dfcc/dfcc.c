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
  bool dump_ast;
  bool dump_asm;
  bool compile_only;
  bool warn_is_error;
} Opts;

static void usage(const char *err_msg) {
  if (err_msg) {
    fprintf(stderr, "error: %s\n", err_msg);
  }
  fprintf(stderr, "usage: dfcc [-h] [options] -o <outfile> <infile>\n");
  if (!err_msg) {
    fprintf(stderr,
      "\n"
      "options:\n"
      "  -h            show this help message and exit\n"
      "  -o outfile    specify the output file\n"
      "  -fdump-ast    dump AST tree\n"
      "  -S            emit assembly\n"
      "  -c            compile the input file but do not link\n"
      "  -std=c11      comply to C11 language standard (default)\n"
      "  -Wall         enable all warnings (default)\n"
      "  -Wpedantic    enable strict ISO C compliance (default)\n"
      "  -Werror       treat warnings as errors\n"
      "  -g            include debug information\n"
    );
  }
  exit(err_msg ? 1 : 0);
}

static void read_f_arg(Opts *opts, char *arg) {
  if (strcmp(arg, "dump-ast") == 0) {
    opts->dump_ast = true;
  } else {
    usage("unknown -f option value");
  }
}

static void read_std_arg(Opts *opts, char *arg) {
  if (strcmp(arg, "td=c11") == 0) {
    /* default */
  } else {
    usage("unknown -std option value");
  }
}

static void read_warning_arg(Opts *opts, char *arg) {
  if (strcmp(arg, "all") == 0) {
    /* default */
  } else if (strcmp(arg, "pedantic") == 0) {
    /* default */
  } else if (strcmp(arg, "error") == 0) {
    opts->warn_is_error = true;
  } else {
    usage("unknown -W option value");
  }
}

static const Opts *read_opts(int argc, char **argv) {
  Opts *opts = calloc(1, sizeof(Opts));
  for (;;) {
    const int opt = getopt(argc, argv, ":ho:fScs:W:g");
    if (opt == -1) break;
    switch (opt) {
    case 'o':
      opts->outpath = optarg;
      break;
    case 'f':
      read_f_arg(opts, optarg);
      break;
    case 'S':
      opts->dump_asm = true;
      break;
    case 's':
      if (strncmp(optarg, "td=", 3) != 0) {
        usage("invalid option");
      }
      read_std_arg(opts, optarg);
      break;
    case 'W':
      read_warning_arg(opts, optarg);
      break;
    case 'h':
      usage(NULL);
      break;
    case 'c':
      opts->compile_only = true;
      break;
    case 'g':
      break;
    default:
      usage("invalid option");
    }
  }
  if (optind != argc - 1) {
    usage("input file required");
  }
  if (!opts->outpath) {
    usage("output file required");
  }
  const int modes = opts->dump_ast + opts->dump_asm + opts->compile_only;
  if (modes > 1) {
    usage("only one of -f, -S and -c must be specified");
  }
  opts->inpath = argv[optind];
  opts->is_stdin = strcmp(opts->inpath, "-") == 0;
  opts->is_stdout = strcmp(opts->outpath, "-") == 0;
  return opts;
}

int main(int argc, char **argv) {
  // Parse opts.
  const Opts *opts = read_opts(argc, argv);
  error_init(opts->warn_is_error);
  stream_push(opts->is_stdin ? NULL : opts->inpath);

  // Tokenize and parse.
  Token *token = cpp();
  Program *prog = parse(token);

  // Generate code.
  gen_offsets(prog);
  gen_prog(prog, opts->is_stdout ? NULL : opts->outpath);

  return 0;
}
