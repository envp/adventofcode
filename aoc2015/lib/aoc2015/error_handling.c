#include "aoc2015/error_handling.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

NORETURN void unreachable(const char *restrict fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, fmt, args);
  va_end(args);
  exit(1);
}
