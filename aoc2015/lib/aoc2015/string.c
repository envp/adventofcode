#include "aoc2015/string.h"

#include <stdlib.h>

string_t str_create_empty() {
  string_t s = {.data = NULL, .length = 0};
  return s;
}

void str_free(string_t str) {
  free(str.data);
  str.data = NULL;
  str.length = 0;
}
