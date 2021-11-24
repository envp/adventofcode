#include "aoc2015/dynamic_array.h"
#include "aoc2015/error_handling.h"

#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

inline static uint64_t pos(int64_t u) {
  if (u <= 0)
    return -2 * u;
  else
    return 2 * u + 1;
}

inline static uint64_t hash_coordinate(int64_t x, int64_t y) {
  x = pos(x);
  y = pos(y);
  return (x + y) * (x + y + 1) / 2 + y;
}

int compare(const void *a, const void *b) {
  const uint64_t *left = a;
  const uint64_t *right = b;
  if (*left < *right) {
    return -1;
  } else if (*left > *right) {
    return 1;
  } else {
    return 0;
  }
}

void part1() {
  dyn_array_t coords = dyn_array_init(sizeof(uint64_t), 0);
  char c;

  int64_t x = 0, y = 0;
  while ((c = getchar()) != EOF) {
    switch (c) {
    case '>':
      x += 1;
      break;
    case '<':
      x -= 1;
      break;
    case '^':
      y += 1;
      break;
    case 'v':
      y -= 1;
      break;
    default:
      break;
    }
    uint64_t hash = hash_coordinate(x, y);
    dyn_array_push(&coords, &hash);
  }

  uint32_t count = 1;

  qsort(coords.blocks, coords.length, coords.item_size, &compare);
  if (coords.length > 0) {
    ++count;
    for (size_t i = 1; i < coords.length; ++i) {
      uint64_t current, prev;
      dyn_array_get(&coords, i - 1, &prev);
      dyn_array_get(&coords, i, &current);
      if (prev != current)
        ++count;
    }
  }
  printf("Part 1: %u\n", count);

  dyn_array_free(&coords);
}

void part2() {}

int main() {
  part1();
  part2();
  return 0;
}
