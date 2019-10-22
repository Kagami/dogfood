#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

typedef enum {
  MR_OBJ,
} MacroKind;

typedef struct {
  MacroKind kind;
  Token *body;
} Macro;

typedef struct {
  const char *include_paths[10];
  int include_npaths;
  Map *macros;
} CppContext;

static CppContext *gCtx;

static Macro *new_macro(MacroKind kind, Token *body) {
  Macro *m = calloc(1, sizeof(Macro));
  m->kind = kind;
  m->body = body;
  return m;
}

static void skip_line() {
  const char *p = stream_pos();
  while (*p != '\n') { p++; }
  stream_setpos(p);
}

static Token *read_ident() {
  Token *tok = lex_one();
  if (tok->kind != TK_IDENT) error_tok(tok, "identifier expected");
  return tok;
}

static void read_define() {
  Token *id = read_ident();
  Token head = { 0 };
  Token *prev = &head;
  for (;;) {
    Token *tok = lex_one();
    if (tok->kind == TK_NEWLINE) break;
    prev->next = tok;
    prev = tok;
  }
  const char *name = strndup(id->str, id->len);
  Macro *macro = new_macro(MR_OBJ, head.next);
  map_put(gCtx->macros, name, macro);
}

static void read_include() {
  const char *hname = lex_header_name();
  hname = hname;
}

static void read_directive(Token *tok) {
  if (token_match(tok, "define")) {
    read_define();
  } else if (token_match(tok, "include")) {
    read_include();
  } else if (token_match(tok, "if")) {
    skip_line();
  } else if (token_match(tok, "endif")) {
    skip_line();
  } else {
    error_tok(tok, "unknown directive");
  }
}

// #define A (2*2)
//   A   + B +   A
// (2*2) + B + (2*2)
static Token *insert_macro(Token *prev, const Macro *m) {
  // Empty macro
  if (!m->body) return prev;

  Token *last;
  prev->next = token_deepcopy(m->body, &last);
  return last;
}

void cpp_init() {
  gCtx = calloc(1, sizeof(CppContext));
  gCtx->macros = new_map();
}

void cpp_add_include_path(const char *ipath) {
  if (gCtx->include_npaths == 10) error("too many include paths");
  gCtx->include_paths[gCtx->include_npaths++] = ipath;
}

Token *cpp() {
  Token head = { 0 };
  Token *prev = &head;

  for (;;) {
    Token *tok = lex_one();
    if (tok->kind == TK_NEWLINE) {
      continue;
    }
    if (tok->kind == TK_IDENT) {
      const Macro *m = map_getbyview(gCtx->macros, tok->str, tok->len);
      if (m) {
        prev = insert_macro(prev, m);
        continue;
      }
    }
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
