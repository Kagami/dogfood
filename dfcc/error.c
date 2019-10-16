#include "dfcc.h"

static char gColorBlack[]   = { 27, '[', '3', '0', ';', '1', 'm', 0 };
static char gColorRed[]     = { 27, '[', '3', '1', ';', '1', 'm', 0 };
static char gColorMagenta[] = { 27, '[', '3', '5', ';', '1', 'm', 0 };
static char gColorCyan[]    = { 27, '[', '3', '6', ';', '1', 'm', 0 };
static char gColorReset[]   = { 27, '[', '0', 'm', 0 };

typedef enum {
  LogLevelNote,
  LogLevelWarning,
  LogLevelError,
} LogLevel;

typedef struct {
  char *filename;
  char *user_input;
} ErrorContext;

static ErrorContext *gCtx;

void error_init(char *filename, char *user_input) {
  gCtx = calloc(1, sizeof(ErrorContext));
  gCtx->filename = filename;
  gCtx->user_input = user_input;
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(LogLevel lvl, char *loc, char *fmt, va_list ap) {
  // TODO(Kagami): Bound checking.
  // Find a line containing `loc`.
  char *line = loc;
  while (gCtx->user_input < line && line[-1] != '\n')
    line--;

  char *end = loc;
  while (*end != '\n')
    end++;

  // Get a line number.
  int line_num = 1;
  for (char *p = gCtx->user_input; p < line; p++)
    if (*p == '\n')
      line_num++;

  // Print out the line.
  fprintf(stderr, "%s", gColorBlack);
  int indent = fprintf(stderr, "%s:%d: ", gCtx->filename, line_num);
  fprintf(stderr, "%s%.*s\n", gColorReset, (int)(end - line), line);

  // Show the error message.
  int pos = loc - line + indent;
  fprintf(stderr, "%*s", pos, ""); // print pos spaces.
  switch (lvl) {
  case LogLevelNote:
    fprintf(stderr, "%s^ note: %s", gColorCyan, gColorReset);
    break;
  case LogLevelWarning:
    fprintf(stderr, "%s^ warning: %s", gColorMagenta, gColorReset);
    break;
  default:
    fprintf(stderr, "%s^ error: %s", gColorRed, gColorReset);
    break;
  }
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
}

// Reports an error location and exit.
void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(LogLevelError, loc, fmt, ap);
  exit(1);
}

// Reports an error location and exit.
void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(LogLevelError, tok->str, fmt, ap);
  exit(1);
}

// Reports an warning location.
void warn_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(LogLevelWarning, tok->str, fmt, ap);
}

// Reports an error and exit.
void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}
