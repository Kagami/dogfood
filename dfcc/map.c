#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

// FNV hash
static uint32_t hash_strbuf(const char *s, size_t len) {
  uint32_t r = 2166136261;
  for (int i = 0; i < len; i++) {
    r ^= s[i];
    r *= 16777619;
  }
  return r;
}

static uint32_t hash_str(const char *s) {
  return hash_strbuf(s, strlen(s));
}

static void maybe_rehash(Map *m) {
  if (!m->keys) {
    m->size = 16;
    m->keys = calloc(m->size, sizeof(char*));
    m->vals = calloc(m->size, sizeof(void*));
    return;
  }
  if (m->nused > m->size*7/10) {
    const size_t size = m->size * 2;
    const char **keys = calloc(size, sizeof(char*));
    const void **vals = calloc(size, sizeof(void*));
    const int mask = size - 1;
    for (int i = 0; i < m->size; i++) {
      if (m->keys[i] == NULL) continue;
      int j = hash_str(m->keys[i]) & mask;
      for (;; j = (j + 1) & mask) {
        if (keys[j] != NULL) continue;
        keys[j] = m->keys[i];
        vals[j] = m->vals[i];
        break;
      }
    }
    m->size = size;
    m->keys = keys;
    m->vals = vals;
  }
}

Map *new_map() {
  Map *m = calloc(1, sizeof(Map));
  return m;
}

const void *map_getbyview(Map *m, const char *key, size_t key_len) {
  if (!m->keys) return NULL;
  const int mask = m->size - 1;
  int i = hash_strbuf(key, key_len) & mask;
  for (; m->keys[i] != NULL; i = (i + 1) & mask) {
    if (strlen(m->keys[i]) == key_len
        && strncmp(m->keys[i], key, key_len) == 0) {
      return m->vals[i];
    }
  }
  return NULL;
}

const void *map_get(Map *m, const char *key) {
  return map_getbyview(m, key, strlen(key));
}

void map_put(Map *m, const char *key, const void *val) {
  maybe_rehash(m);
  const int mask = m->size - 1;
  int i = hash_str(key) & mask;
  for (;; i = (i + 1) & mask) {
    if (m->keys[i] == NULL) {
      m->keys[i] = key;
      m->vals[i] = val;
      m->nused++;
      return;
    }
    if (strcmp(m->keys[i], key) == 0) {
      m->vals[i] = val;
      return;
    }
  }
}
