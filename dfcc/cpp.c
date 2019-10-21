#include <stdlib.h>
#include "dfcc.h"

typedef struct {
  Map *macros;
} CppContext;

static CppContext *gCtx;

Token *cpp() {
  gCtx = calloc(1, sizeof(CppContext));
  gCtx->macros = new_map();

  Token head = { 0 };
  Token *cur = &head;
  for (;;) {
    cur = lex_one(cur);
    if (cur->kind == TK_EOF) {
      if (!stream_pop()) break;
    }
  }
  return head.next;
}
