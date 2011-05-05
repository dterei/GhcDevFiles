#include "c_hash.h"

long c_hash(const char* str, long len) {
  long hash = 0;

  for (;len; --len) {
    hash = (hash * 33) ^ *str++;
  }

  return hash;
}
