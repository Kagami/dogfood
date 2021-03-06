#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "dfcc.h"

// Create a new token.
Token *new_token(TokenKind kind, const char *str, int len) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
  tok->origin = stream_head();
  return tok;
}

Token *token_copy(Token *orig) {
  Token *tok = malloc(sizeof(Token));
  memcpy(tok, orig, sizeof(Token));
  return tok;
}

Token *token_deepcopy(Token *orig, Token **last) {
  Token *head = token_copy(orig);
  Token *tok = head;
  while (tok->next) {
    tok = tok->next = token_copy(tok->next);
  }
  *last = tok;
  return head;
}

bool token_match(Token *tok, const char *str) {
  return strlen(str) == tok->len &&
         strncmp(tok->str, str, tok->len) == 0;
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
  tok->contents = strndup(buf, len);
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

// Skip adjacent whitespace/comment sequences, but stop at newline.
static void skip_space(const char *p, const char **end) {
  bool skipped;
  do {
    skipped = false;
    // Whitespace characters
    if (isspace(*p)) {
      skipped = true;
      do {
        if (*p == '\n') {
          *end = p;
          return;
        }
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
    // Block comments
    if (startswith(p, "/*")) {
      skipped = true;
      char *q = strstr(p + 2, "*/");
      if (!q) error_at(p, "unclosed block comment");
      p = q + 2;
    }
  } while (skipped);
  *end = p;
}

// Consume header name from the current stream.
Token *lex_headername(bool *is_angle) {
  const char *p = stream_pos();
  skip_space(p, &p);
  char close;
  if (*p == '<') {
    close = '>';
    *is_angle = true;
  } else if (*p == '"') {
    close = '"';
    *is_angle = false;
  } else {
    // Don't support macro-expanded path currently.
    error_at(p, "header name expected");
  }
  const char *q = ++p;
  while (*p != close) {
    if (*p == '\n') error_at(p, "premature end of header name");
    p++;
  }
  p++;
  const int name_len = p - q - 1;
  if (name_len == 0) error_at(q, "empty header name");
  skip_space(p, &p);
  stream_setpos(p);

  Token *tok = lex_one();
  if (tok->kind != TK_NEWLINE) error_tok(tok, "newline expected");

  Token *htok = new_token(TK_STR, q, name_len);
  htok->contents = strndup(q, name_len);
  htok->cont_len = name_len + 1;
  return htok;
}

// Consume single token from the current stream.
Token *lex_one() {
  Token *tok;
  const char *kw;
  const char *p = stream_pos();
  skip_space(p, &p);

  // EOF
  if (!*p) {
    tok = new_token(TK_EOF, p, 0);
  // Newline
  } else if (*p == '\n') {
    tok = new_token(TK_NEWLINE, p, 1);
    p++;
  // Preprocessing directive
  } else if (*p == '#' && stream_atbol()) {
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

  stream_setpos(p);
  return tok;
}
