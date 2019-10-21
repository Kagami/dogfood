#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "dfcc.h"

bool token_match(Token *tok, const char *str) {
  return strncmp(tok->str, str, tok->len) == 0;
}

// Create a new token.
static Token *new_token(TokenKind kind, const char *str, int len) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
  tok->origin = stream_peek();
  return tok;
}

static bool startswith(const char *p, const char *q) {
  return strncmp(p, q, strlen(q)) == 0;
}

static bool is_ident_start(char c) {
  return isalpha(c) || c == '_';
}

static bool is_ident(char c) {
  return is_ident_start(c) || isdigit(c);
}

static const char *read_reserved(const char *p) {
  // Keyword
  static const char *kws[] = {
    "return", "if", "else", "while", "for", "int",
    "char", "sizeof", "struct", "typedef", "short",
    "long", "void", "_Bool", "enum", "static", "break",
    "continue", "goto", "switch", "case", "default",
    "extern", "_Alignof", "do", "signed", "unsigned",
    "const",
  };
  static const int kw_len = sizeof(kws) / sizeof(*kws);
  for (int i = 0; i < kw_len; i++) {
    const int len = strlen(kws[i]);
    if (startswith(p, kws[i]) && !is_ident(p[len])) {
      return kws[i];
    }
  }

  // Multi-letter punctuator
  static const char *ops[] = {
    "<<=", ">>=", "...", "==", "!=", "<=", ">=",
    "->", "++", "--", "<<", ">>", "+=", "-=", "*=",
    "/=", "&&", "||", "&=", "|=", "^=",
  };
  static const int op_len = sizeof(ops) / sizeof(*ops);
  for (int i = 0; i < op_len; i++) {
    if (startswith(p, ops[i])) {
      return ops[i];
    }
  }

  return NULL;
}

static char read_oct_char(char c, const char *p, const char **end) {
  for (int i = 0; i < 2; i++) {
    int d = *p - '0';
    if (d < 0 || d > 7) break;
    c = (c << 3) | d;
    p++;
  }
  *end = p;
  return c;
}

static char read_escape_char(const char *p, const char **end) {
  char c = *p++;
  *end = p;
  switch (c) {
  case '\'': case '"': case '?': case '\\': return c;
  case 'a': return '\a';
  case 'b': return '\b';
  case 'f': return '\f';
  case 'n': return '\n';
  case 'r': return '\r';
  case 't': return '\t';
  case 'v': return '\v';
  }
  int d = c - '0';
  if (d >= 0 && d <= 7) return read_oct_char(d, p, end);
  warn_at(p - 1, "unknown escape sequence");
  return c;
}

static Token *read_string_literal(const char *start) {
  const char *p = start + 1;
  char buf[1024];
  int len = 0;

  for (;;) {
    if (len == sizeof(buf))
      error_at(start, "string literal too large");
    if (*p == '\0')
      error_at(start, "unclosed string literal");
    if (*p == '"')
      break;

    if (*p == '\\') {
      p++;
      buf[len++] = read_escape_char(p, &p);
    } else {
      buf[len++] = *p++;
    }
  }

  Token *tok = new_token(TK_STR, start, p - start + 1);
  tok->contents = malloc(len + 1);
  memcpy(tok->contents, buf, len);
  tok->contents[len] = '\0';
  tok->cont_len = len + 1;
  return tok;
}

static Token *read_char_literal(const char *start) {
  const char *p = start + 1;
  if (*p == '\0')
    error_at(start, "unclosed char literal");

  char c;
  if (*p == '\\') {
    p++;
    c = read_escape_char(p, &p);
  } else {
    c = *p++;
  }

  if (*p != '\'')
    error_at(start, "char literal too long");
  p++;

  Token *tok = new_token(TK_NUM, start, p - start);
  tok->val = c;
  tok->ty = gIntType;
  return tok;
}

static Token *read_int_literal(const char *start) {
  const char *p = start;

  // Read a binary, octal, decimal or hexadecimal number.
  int base;
  if (!strncasecmp(p, "0x", 2) && is_ident(p[2])) {
    p += 2;
    base = 16;
  } else if (!strncasecmp(p, "0b", 2) && is_ident(p[2])) {
    p += 2;
    base = 2;
  } else if (*p == '0') {
    base = 8;
  } else {
    base = 10;
  }

  long val = strtol(p, (char**)&p, base);
  Type *ty = gIntType;

  // Read L, LL, LU, LLU, U, UL, ULL prefix or infer a type.
  if (*p == 'L' || *p == 'l') {
    p++;
    ty = gLongType;
    if (*p == 'L' || *p == 'l') { p++; }
    if (*p == 'U' || *p == 'u') { p++; }
  } else if (*p == 'U' || *p == 'u') {
    p++;
    if (*p == 'L' || *p == 'l') { p++; ty = gLongType; }
    if (*p == 'L' || *p == 'l') { p++; }
  } else if (val != (int)val) {
    ty = gLongType;
  }

  if (is_ident(*p)) {
    error_at(p, "invalid digit");
  }

  Token *tok = new_token(TK_NUM, start, p - start);
  tok->val = val;
  tok->ty = ty;
  return tok;
}

// Skip all adjacent whitespace/comment sequences and return BOL flag.
static bool skip_space(const char *p, const char **end) {
  bool bol = false;
  bool skipped;
  do {
    skipped = false;
    // Whitespace characters
    if (isspace(*p)) {
      skipped = true;
      do {
        if (*p == '\n') { bol = true; }
        p++;
      } while (isspace(*p));
    }
    // Line comments
    if (startswith(p, "//")) {
      skipped = true;
      p += 2;
      while (*p != '\n') {
        p++;
      }
    }
    // Block comments; this clears BOL flag
    if (startswith(p, "/*")) {
      skipped = true;
      bol = false;
      char *q = strstr(p + 2, "*/");
      if (!q) error_at(p, "unclosed block comment");
      p = q + 2;
    }
  } while (skipped);
  *end = p;
  return bol;
}

// Read single token from the current stream.
Token *lex_one() {
  const char *p = stream_pos();
  const char *kw;
  Token *tok;

  bool bol = skip_space(p, &p);
  if (!*p) return new_token(TK_EOF, p, 0);

  // Preprocessing directive
  if (*p == '#' && bol) {
    p++;
    skip_space(p, &p);
    if (!is_ident_start(*p)) error_at(p, "unknown directive");
    const char *q = p++;
    while (is_ident(*p)) {
      p++;
    }
    tok = new_token(TK_DIRECTIVE, q, p - q);
  // String literal
  } else if (*p == '"') {
    tok = read_string_literal(p);
    p += tok->len;
  // Character literal
  } else if (*p == '\'') {
    tok = read_char_literal(p);
    p += tok->len;
  // Keywords or multi-letter punctuators
  } else if ((kw = read_reserved(p))) {
    int len = strlen(kw);
    tok = new_token(TK_RESERVED, p, len);
    p += len;
  // Identifier
  } else if (is_ident_start(*p)) {
    const char *q = p++;
    while (is_ident(*p)) {
      p++;
    }
    tok = new_token(TK_IDENT, q, p - q);
  // Single-letter punctuators
  } else if (ispunct(*p)) {
    tok = new_token(TK_RESERVED, p++, 1);
  // Integer literal
  } else if (isdigit(*p)) {
    tok = read_int_literal(p);
    p += tok->len;
  // Wrong input
  } else {
    error_at(p, "invalid token");
  }

  stream_pos_set(p);
  return tok;
}
