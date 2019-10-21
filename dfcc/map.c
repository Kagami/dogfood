#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

static uint32_t fnv_hash(char *s) {
  uint32_t r = 2166136261;
  for (; *s; s++) {
    r ^= *s;
    r *= 16777619;
  }
  return r;
}

Map *new_map(void) {
  Map *m = calloc(1, sizeof(Map));
  m->size = 100;
  m->key = calloc(m->size, sizeof(char*));
  m->val = calloc(m->size, sizeof(void*));
  return m;
}

void *map_get(Map *m, char *key) {
  const int mask = m->size - 1;
  int i = fnv_hash(key) & mask;
  for (; m->key[i] != NULL; i = (i + 1) & mask) {
    if (strcmp(m->key[i], key) == 0) {
      return m->val[i];
    }
  }
  return NULL;
}

// FIXME(Kagami): Realloc.
void map_put(Map *m, char *key, void *val) {
  const int mask = m->size - 1;
  int i = fnv_hash(key) & mask;
  for (;; i = (i + 1) & mask) {
    if (m->key[i] == NULL) {
      m->key[i] = key;
      m->val[i] = val;
      return;
    }
    if (strcmp(m->key[i], key) == 0) {
      m->val[i] = val;
      return;
    }
  }
}
