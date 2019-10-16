#include "dfcc.h"

static char *gFilename = "(unknown)";
static char *gUserInput = { 0 };

void error_init(char *filename, char *user_input) {
  gFilename = filename;
  gUserInput = user_input;
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(char *loc, char *fmt, va_list ap) {
  // TODO(Kagami): Bound checking.
  // Find a line containing `loc`.
  char *line = loc;
  while (gUserInput < line && line[-1] != '\n')
    line--;

  char *end = loc;
  while (*end != '\n')
    end++;

  // Get a line number.
  int line_num = 1;
  for (char *p = gUserInput; p < line; p++)
    if (*p == '\n')
      line_num++;

  // Print out the line.
  int indent = fprintf(stderr, "%s:%d: ", gFilename, line_num);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  // Show the error message.
  int pos = loc - line + indent;
  fprintf(stderr, "%*s", pos, ""); // print pos spaces.
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
}

// Reports an error and exit.
void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

// Reports an error location and exit.
void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(loc, fmt, ap);
  exit(1);
}

// Reports an error location and exit.
void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->str, fmt, ap);
  exit(1);
}

// Reports an warning location.
void warn_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->str, fmt, ap);
}
