#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

typedef struct {
  Stream *head;
  // Cache all opened streams, we don't need to read them again.
  // Technically with include guards this shouldn't happen but some code
  // might miss them.
  Map *cache;
} StreamContext;

static StreamContext *gCtx;

static const char *read_file(const char *path, const char *name) {
  FILE *fp;
  long fsize;
  if (path) {
    fp = fopen(path, "rb");
    if (!fp) error("cannot open %s (%s)", name, strerror(errno));
    // This is UB per C standard but OK per POSIX
    fseek(fp, 0, SEEK_END);
    fsize = ftell(fp);
    rewind(fp);
  } else {
    fp = stdin;
    // Read max 100kb from stdin. Might realloc array or use streaming
    // parsing instead but keep it simple for now.
    fsize = 100 * 1024;
  }

  char *buf = malloc(fsize + 2);
  size_t rsize = fread(buf, 1, fsize + 1, fp); // one more to reach EOF
  if (ferror(fp)) error("cannot read %s (%s)", name, strerror(errno));
  if (!feof(fp)) error("cannot read %s (file too large)", name);
  if (path) {
    fclose(fp);
  }

  // Make sure that the string ends with "\n\0"
  if (rsize == 0 || buf[rsize] != '\n') {
    buf[rsize++] = '\n';
  }
  buf[rsize] = '\0';
  return buf;
}

static void stream_init() {
  gCtx = calloc(1, sizeof(StreamContext));
  gCtx->cache = new_map();
}

void stream_push(const char *path) {
  if (!gCtx) { stream_init(); }
  Stream *s = calloc(1, sizeof(Stream));
  s->path = path;
  s->name = path ? path : "stdin";
  const Stream *cached = map_get(gCtx->cache, s->name);
  if (cached) {
    s->pos = s->contents = cached->contents;
  } else {
    s->pos = s->contents = read_file(path, s->name);
    map_put(gCtx->cache, s->name, s);
  }
  s->prev = gCtx->head;
  gCtx->head = s;
}

Stream *stream_pop() {
  gCtx->head = gCtx->head->prev;
  return gCtx->head;
}

Stream *stream_head() {
  return gCtx->head;
}

const char *stream_path() {
  return stream_head()->path;
}

const char *stream_pos() {
  return stream_head()->pos;
}

void stream_setpos(const char *pos) {
  stream_head()->pos = pos;
}

bool stream_atbol() {
  Stream *s = stream_head();
  return s->pos == s->contents || s->pos[-1] == '\n';
}
