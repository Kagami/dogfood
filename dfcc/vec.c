#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

static void extend_vec(Vec *v, size_t n) {
  if (v->size + n > v->cap) {
    v->cap = v->cap * 2 + n;
    v->elems = realloc(v->elems, v->cap * sizeof(void*));
  }
}

Vec *new_vec() {
  Vec *v = calloc(1, sizeof(Vec));
  return v;
}

void vec_push(Vec *v, const void *elem) {
  extend_vec(v, 1);
  v->elems[v->size++] = elem;
}

//---

static void extend_buf(Buf *b, size_t n) {
  if (b->size + n > b->cap) {
    b->cap = b->cap * 2 + n;
    b->data = realloc(b->data, b->cap);
  }
}

Buf *new_buf() {
  Buf *b = calloc(1, sizeof(Buf));
  return b;
}

size_t buf_write(Buf *b, const void *chunk, size_t chunk_size) {
  extend_buf(b, chunk_size);
  memcpy(b->data + b->size, chunk, chunk_size);
  const size_t old_size = b->size;
  b->size += chunk_size;
  return old_size;
}

size_t buf_writestr(Buf *b, const char *str) {
  return buf_write(b, str, strlen(str) + 1);
}
