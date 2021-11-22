#include "aoc2015/error_handling.h"
#include "aoc2015/string.h"

#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

bool read_dimensions(uint32_t sides[3]) {
  string_t line = str_create_empty();
  ssize_t nread = getline(&line.data, &line.length, stdin);
  if (nread == EOF) {
    return false;
  }
  uint32_t num_matches =
      sscanf(line.data, "%dx%dx%d", &sides[0], &sides[1], &sides[2]);
  assert(num_matches == 3 && "Unexpected input");
  str_free(line);
  return true;
}

int compare(const void *a, const void *b) {
  uint32_t *x = (uint32_t *)a;
  uint32_t *y = (uint32_t *)b;
  if (*x > *y) {
    return 1;
  } else if (*x < *y) {
    return -1;
  } else {
    return 0;
  }
}

int main() {
  uint32_t total_area = 0;
  uint32_t ribbon_length = 0;

  uint32_t sides[3] = {0};
  while (read_dimensions(sides)) {
    qsort(&sides, 3, sizeof(uint32_t), &compare);
    uint32_t surface_area =
        2 * (sides[0] * sides[1] + sides[1] * sides[2] + sides[2] * sides[0]);
    uint32_t slack = sides[0] * sides[1];
    total_area += surface_area + slack;
    ribbon_length +=
        2 * (sides[0] + sides[1]) + (sides[0] * sides[1] * sides[2]);
  }

  printf("Part 1: %d\n", total_area);
  printf("Part 2: %d\n", ribbon_length);
}
