#include "aoc2015/error_handling.h"

#include <stdio.h>

int main() {
  char c;
  int count = 0;
  int idx = 0;
  int first_basement_idx = -1;
  while ((c = getchar()) != EOF) {
    ++idx;
    switch (c) {
    case '(':
      count += 1;
      break;
    case ')':
      count -= 1;
      if (count < 0 && first_basement_idx == -1) {
        first_basement_idx = idx;
      }
      break;
    case '\n':
      break;
    default:
      unreachable("Unknown char: %d\n", c);
      break;
    }
  }
  printf("Part 1: %d\n", count);
  printf("Part 2: %d\n", first_basement_idx);
  return 0;
}
