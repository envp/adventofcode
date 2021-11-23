#ifndef AOC2015_STRING_H
#define AOC2015_STRING_H

#include <stddef.h>

/// Struct that holds an owned pointer to char array
typedef struct string_t {
  char* data;
  size_t length;
} string_t;

/// Create an empty string
string_t str_create_empty();

/// Free the data that this string holds
void str_free(string_t);

#endif /* AOC2015_STRING_H */
