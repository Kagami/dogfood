#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "dfcc.h"

// Create a new token and add it as the next token of `cur`.
static Token *new_token(TokenKind kind, Token *cur, const char *str, int len) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
  tok->origin = stream_back();
  cur->next = tok;
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

static const char *starts_with_reserved(const char *p) {
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
    if (startswith(p, ops[i])) return ops[i];
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

static Token *read_string_literal(Token *cur, const char *start) {
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

  Token *tok = new_token(TK_STR, cur, start, p - start + 1);
  tok->contents = malloc(len + 1);
  memcpy(tok->contents, buf, len);
  tok->contents[len] = '\0';
  tok->cont_len = len + 1;
  return tok;
}

static Token *read_char_literal(Token *cur, const char *start) {
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

  Token *tok = new_token(TK_NUM, cur, start, p - start);
  tok->val = c;
  tok->ty = gIntType;
  return tok;
}

static Token *read_int_literal(Token *cur, const char *start) {
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

  Token *tok = new_token(TK_NUM, cur, start, p - start);
  tok->val = val;
  tok->ty = ty;
  return tok;
}

// Read single token from the current stream.
Token *lex_one(Token *cur) {
  const char *p = stream_back()->pos;
  const char *kw;
  // EOF
  if (!*p) {
    cur = new_token(TK_EOF, cur, p, 0);
  // Skip whitespace characters
  } else if (isspace(*p)) {
    p++;
  // Skip line comments
  } else if (startswith(p, "//")) {
    p += 2;
    while (*p != '\n') {
      p++;
    }
  // Skip block comments
  } else if (startswith(p, "/*")) {
    char *q = strstr(p + 2, "*/");
    if (!q) error_at(p, "unclosed block comment");
    p = q + 2;
  // String literal
  } else if (*p == '"') {
    cur = read_string_literal(cur, p);
    p += cur->len;
  // Character literal
  } else if (*p == '\'') {
    cur = read_char_literal(cur, p);
    p += cur->len;
  // Keywords or multi-letter punctuators
  } else if ((kw = starts_with_reserved(p))) {
    int len = strlen(kw);
    cur = new_token(TK_RESERVED, cur, p, len);
    p += len;
  // Identifier
  } else if (is_ident_start(*p)) {
    const char *q = p++;
    while (is_ident(*p)) {
      p++;
    }
    cur = new_token(TK_IDENT, cur, q, p - q);
  // Single-letter punctuators
  } else if (ispunct(*p)) {
    cur = new_token(TK_RESERVED, cur, p++, 1);
  // Integer literal
  } else if (isdigit(*p)) {
    cur = read_int_literal(cur, p);
    p += cur->len;
  // Wrong input
  } else {
    error_at(p, "invalid token");
  }
  stream_back()->pos = p;
  return cur;
}
