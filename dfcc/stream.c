#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "dfcc.h"

static Stream *gStream;

static const char *read_file(const char *path, FILE *fp) {
  static const int filemax = 100 * 1024;
  char *buf = malloc(filemax);
  int size = fread(buf, 1, filemax - 2, fp);
  if (!feof(fp)) error("%s: file too large", path);

  // Make sure that the string ends with "\n\0"
  if (size == 0 || buf[size - 1] != '\n') {
    buf[size++] = '\n';
  }
  buf[size] = '\0';
  return buf;
}

void stream_push(const char *path) {
  Stream *s = calloc(1, sizeof(Stream));
  s->path = path ? path : "stdin";
  s->fp = path ? fopen(path, "rb") : stdin;
  if (!s->fp) error("cannot open %s: %s", s->path, strerror(errno));
  s->pos = s->contents = read_file(s->path, s->fp);
  s->prev = gStream;
  gStream = s;
}

Stream *stream_pop() {
  gStream = gStream->prev;
  return gStream;
}

Stream *stream_back() {
  return gStream;
}
