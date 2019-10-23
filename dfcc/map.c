#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

static uint32_t fnv_hash(const char *s, size_t key_len) {
  uint32_t r = 2166136261;
  for (int i = 0; i < key_len; i++) {
    r ^= s[i];
    r *= 16777619;
  }
  return r;
}

Map *new_map() {
  Map *m = calloc(1, sizeof(Map));
  m->size = 128;
  m->key = calloc(m->size, sizeof(char*));
  m->val = calloc(m->size, sizeof(void*));
  return m;
}

// TODO(Kagami): Store len for m->key?
const void *map_getbyview(Map *m, const char *key, size_t key_len) {
  const int mask = m->size - 1;
  int i = fnv_hash(key, key_len) & mask;
  for (; m->key[i] != NULL; i = (i + 1) & mask) {
    if (strlen(m->key[i]) == key_len
        && strncmp(m->key[i], key, key_len) == 0) {
      return m->val[i];
    }
  }
  return NULL;
}

const void *map_get(Map *m, const char *key) {
  return map_getbyview(m, key, strlen(key));
}

// FIXME(Kagami): Realloc.
void map_put(Map *m, const char *key, const void *val) {
  const int mask = m->size - 1;
  int i = fnv_hash(key, strlen(key)) & mask;
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
