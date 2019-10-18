#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "dfcc.h"

// Returns the contents of a given file.
static char *read_file(char *path, bool is_stdin) {
  FILE *fp;
  if (is_stdin) {
    fp = stdin;
  } else {
    // Open and read the file.
    fp = fopen(path, "r");
    if (!fp) error("cannot open %s: %s", path, strerror(errno));
  }

  int filemax = 10 * 1024 * 1024;
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
  if (argc != 2) {
    fprintf(stderr, "usage: %s <infile>\n", argv[0]);
    exit(1);
  }

  // Prepare input.
  char *filename = argv[1];
  bool is_stdin = strcmp(filename, "-") == 0;
  if (is_stdin) {
    filename = "stdin";
  }
  char *user_input = read_file(filename, is_stdin);
  error_init(filename, user_input);

  // Tokenize and parse.
  Token *token = lex(user_input);
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
