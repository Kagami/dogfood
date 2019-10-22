#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "dfcc.h"

static Stream *gStream;

static const char *read_file(const char *path, const char *name) {
  FILE *fp;
  long fsize;
  if (path) {
    fp = fopen(path, "rb");
    if (!fp) error("cannot open %s: %s", name, strerror(errno));
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
  if (ferror(fp)) error("cannot read %s: %s", name, strerror(errno));
  if (!feof(fp)) error("cannot read %s: file too large", name);
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

void stream_push(const char *path) {
  Stream *s = calloc(1, sizeof(Stream));
  s->name = path ? path : "stdin";
  s->pos = s->contents = read_file(path, s->name);
  s->prev = gStream;
  gStream = s;
}

Stream *stream_pop() {
  gStream = gStream->prev;
  return gStream;
}

Stream *stream_peek() {
  return gStream;
}

const char *stream_pos() {
  return stream_peek()->pos;
}

void stream_pos_set(const char *pos) {
  stream_peek()->pos = pos;
}

bool stream_at_bol() {
  Stream *s = stream_peek();
  return s->pos == s->contents || s->pos[-1] == '\n';
}
