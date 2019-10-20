#include "dfcc.h"

// Preprocess input.
Token *cpp(const char *src) {
  Token head = { 0 };
  Token *tok = &head;
  for (;;) {
    tok = lex_one(src, &src, tok);
    if (tok->kind == TK_EOF) break;
  }
  return head.next;
}
