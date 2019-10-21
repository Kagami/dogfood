#include <stdlib.h>
#include "dfcc.h"

typedef struct {
  Map *macros;
} CppContext;

static CppContext *gCtx;

static void directive(Token *tok) {
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
      directive(tok);
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
