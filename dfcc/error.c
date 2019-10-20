#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include "dfcc.h"

static const char gColorBlack[]   = { 27, '[', '3', '0', ';', '1', 'm', 0 };
static const char gColorRed[]     = { 27, '[', '3', '1', ';', '1', 'm', 0 };
static const char gColorMagenta[] = { 27, '[', '3', '5', ';', '1', 'm', 0 };
static const char gColorCyan[]    = { 27, '[', '3', '6', ';', '1', 'm', 0 };
static const char gColorReset[]   = { 27, '[', '0', 'm', 0 };

typedef enum {
  LogLevelNote,
  LogLevelWarning,
  LogLevelError,
} LogLevel;

typedef struct {
  const char *filename;
  const char *user_input;
  bool werror;
} ErrorContext;

static ErrorContext *gCtx;

void error_init(const char *filename, const char *user_input, bool werror) {
  gCtx = calloc(1, sizeof(ErrorContext));
  gCtx->filename = filename;
  gCtx->user_input = user_input;
  gCtx->werror = werror;
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(LogLevel lvl, const char *loc, const char *fmt, va_list ap) {
  // TODO(Kagami): Bound checking.
  // Find a line containing `loc`.
  const char *line = loc;
  while (gCtx->user_input < line && line[-1] != '\n') {
    line--;
  }

  const char *end = loc;
  while (*end != '\n') {
    end++;
  }

  // Get a line number.
  int line_num = 1;
  for (const char *p = gCtx->user_input; p < line; p++) {
    if (*p == '\n') {
      line_num++;
    }
  }

  // Print out the line.
  fprintf(stderr, "\r%s", gColorBlack);
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
void error_at(const char *loc, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(LogLevelError, loc, fmt, ap);
  va_end(ap);
  exit(1);
}

// Reports an error location and exit.
void error_tok(Token *tok, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(LogLevelError, tok->str, fmt, ap);
  va_end(ap);
  exit(1);
}

// Reports an warning location.
void warn_tok(Token *tok, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(gCtx->werror ? LogLevelError : LogLevelWarning, tok->str, fmt, ap);
  va_end(ap);
  if (gCtx->werror) {
    exit(1);
  }
}

// Reports an error and exit.
void error(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}
