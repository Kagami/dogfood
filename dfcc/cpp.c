#include <stdlib.h>
#include "dfcc.h"

typedef struct {
  Map *macros;
} CppContext;

static CppContext *gCtx;

static void skip_line() {
  const char *p = stream_pos();
  while (*p != '\n') {
    p++;
  }
  stream_pos_set(p);
}

static void read_directive(Token *tok) {
  if (token_match(tok, "define")) {
    skip_line();
  } else if (token_match(tok, "if")) {
    skip_line();
  } else if (token_match(tok, "endif")) {
    skip_line();
  } else if (token_match(tok, "include")) {
    skip_line();
  } else {
    error_tok(tok, "unknown directive");
  }
}

Token *cpp() {
  gCtx = calloc(1, sizeof(CppContext));
  gCtx->macros = new_map();

  Token head = { 0 };
  Token *prev = &head;
  Token *tok;

  for (;;) {
    tok = lex_one();
    if (tok->kind == TK_DIRECTIVE) {
      read_directive(tok);
      continue;
    }
    if (tok->kind == TK_EOF) {
      if (!stream_pop()) {
        prev->next = tok;
        break;
      }
      continue;
    }
    prev->next = tok;
    prev = tok;
  }

  return head.next;
}
