#include <libgen.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "dfcc.h"

typedef enum {
  MR_OBJ,
} MacroKind;

typedef struct {
  MacroKind kind;
  Token *body;
} Macro;

typedef struct {
  const char *include_dirs[10];
  int include_ndirs;
  Map *macros;
} CppContext;

static CppContext *gCtx;

static Macro *new_macro(MacroKind kind, Token *body) {
  Macro *m = calloc(1, sizeof(Macro));
  m->kind = kind;
  m->body = body;
  return m;
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

// TODO(Kagami): Normalize path.
static char *join(const char *dir, const char *name) {
  char *path = calloc(1, strlen(dir) + 1/*sep*/ + strlen(name) + 1/*0*/);
  strcat(path, dir);
  strcat(path, "/");
  strcat(path, name);
  return path;
}

static bool check_header(const char *path) {
  // It would be better to just fopen and pass fp to the stream module
  // to avoid race condition but it needs filename anyway (to log
  // errors) and caches already opened files so leave it this way.
  return access(path, R_OK) != -1;
}

static void read_include() {
  bool is_global;
  Token *htok = lex_headername(&is_global);
  const char *hname = htok->contents;
  // Search by its name relative to source path
  if (!is_global) {
    const char *spath = stream_path();
    const char *hdir = spath ? dirname(strdup(spath)) : ".";
    const char *hpath = join(hdir, hname);
    if (check_header(hpath)) {
      stream_push(hpath);
      return;
    }
  }
  // Search in include dirs
  for (int i = 0; i < gCtx->include_ndirs; i++) {
    const char *hpath = join(gCtx->include_dirs[i], hname);
    if (check_header(hpath)) {
      stream_push(hpath);
      return;
    }
  }
  error_tok(htok, "cannot find header");
}

static void read_directive(Token *tok) {
  if (token_match(tok, "define")) {
    read_define();
  } else if (token_match(tok, "include")) {
    read_include();
  } else if (token_match(tok, "if")
             || token_match(tok, "ifdef")
             || token_match(tok, "ifndef")
             || token_match(tok, "endif")
             || token_match(tok, "undef")) {
    // Not supported currently
    const char *p = stream_pos();
    while (*p != '\n') { p++; }
    stream_setpos(p);
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

static void cpp_init() {
  gCtx = calloc(1, sizeof(CppContext));
  gCtx->macros = new_map();
}

void cpp_idir(const char *dir) {
  if (!gCtx) { cpp_init(); }
  if (gCtx->include_ndirs == 10) error("too many include dirs");
  gCtx->include_dirs[gCtx->include_ndirs++] = dir;
}

Token *cpp() {
  if (!gCtx) { cpp_init(); }
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
