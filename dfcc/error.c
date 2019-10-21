#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include "dfcc.h"

static const char *gColorBlack   = "\33[30;1m";
static const char *gColorRed     = "\33[31;1m";
static const char *gColorMagenta = "\33[35;1m";
static const char *gColorCyan    = "\33[36;1m";
static const char *gColorReset   = "\33[0m";

typedef enum {
  LogLevelNote,
  LogLevelWarning,
  LogLevelError,
} LogLevel;

typedef struct {
  bool warn_is_error;
} ErrorContext;

static ErrorContext *gCtx;

void error_init(bool warn_is_error) {
  gCtx = calloc(1, sizeof(ErrorContext));
  gCtx->warn_is_error = warn_is_error;
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(LogLevel lvl, Stream *origin, const char *loc, const char *fmt, va_list ap) {
  // TODO(Kagami): Bound checking.
  // Find a line containing `loc`.
  const char *line = loc;
  while (origin->contents < line && line[-1] != '\n') {
    line--;
  }

  const char *end = loc;
  while (*end != '\0' && *end != '\n') {
    end++;
  }

  // Get a line number.
  int line_num = 1;
  for (const char *p = origin->contents; p < line; p++) {
    if (*p == '\n') {
      line_num++;
    }
  }

  // Print out the line.
  fprintf(stderr, "\r%s", gColorBlack);
  int indent = fprintf(stderr, "%s:%d: ", origin->path, line_num);
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
  verror_at(LogLevelError, stream_back(), loc, fmt, ap);
  va_end(ap);
  exit(1);
}

// Reports an error token and exit.
void error_tok(Token *tok, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(LogLevelError, tok->origin, tok->str, fmt, ap);
  va_end(ap);
  exit(1);
}

// Reports a warning location.
void warn_at(const char *loc, const char *fmt, ...) {
  const int lvl = gCtx->warn_is_error ? LogLevelError : LogLevelWarning;
  va_list ap;
  va_start(ap, fmt);
  verror_at(lvl, stream_back(), loc, fmt, ap);
  va_end(ap);
  if (gCtx->warn_is_error) {
    exit(1);
  }
}

// Reports a warning token.
void warn_tok(Token *tok, const char *fmt, ...) {
  const int lvl = gCtx->warn_is_error ? LogLevelError : LogLevelWarning;
  va_list ap;
  va_start(ap, fmt);
  verror_at(lvl, tok->origin, tok->str, fmt, ap);
  va_end(ap);
  if (gCtx->warn_is_error) {
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
