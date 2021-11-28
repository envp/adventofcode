#include "aoc2015/dynamic_array.h"
#include "aoc2015/error_handling.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

/// Scale the capacity by intrinsic the growth factor
inline static size_t next_size(size_t current) {
  return current + current / 2;
}

/// Return the largest prev_size, such that:
/// ```
/// next_size(prev_size(x)) <= x
/// ```
inline static size_t prev_size(size_t current) {
  if (current <= 24) {
    return 16;
  }
  return (current / 3) * 2;
}

/// Ensure the given array has enough capacity to insert the next elem
/// This will trigger an O(n) re-allocation every time the array length reaches
/// a certain fraction of its capacity
static bool ensure_capacity(dyn_array_t *array) {
  if (array->length == array->capacity) {
    const size_t new_capacity = next_size(array->capacity);
    array->items = realloc(array->items, new_capacity * array->item_size);
    /// Clear the new allocated memory
    if (errno != ENOMEM) {
      size_t num_allocated = new_capacity - array->capacity;
      memset(array->items + array->capacity, 0,
             array->item_size * num_allocated);
    }
    // realloc(...) doesn't change the array upon failure, but does set errno
    // https://linux.die.net/man/3/realloc
    return errno != ENOMEM;
  }
  return true;
}

inline static void pop_unchecked(dyn_array_t *array, void *dest) {
  size_t end_offset = array->length * array->item_size;
  size_t new_end_offset = (array->length - 1) * array->item_size;
  memmove(dest, array->items + end_offset, array->item_size);
  memset(array->items + new_end_offset, 0, array->item_size);
}

/// Shrink the array if the length under a certain fraction of the capacity
inline static void shrink(dyn_array_t *array) {
  size_t threshold = prev_size(array->capacity);
  if (array->length >= threshold) {
    return;
  }
  array->items = realloc(array->items, threshold * array->item_size);
  if (errno == ENOMEM) {
    unreachable("Unable to de-allocate memory?!");
  }
}

dyn_array_t dyn_array_init(size_t item_size, size_t capacity) {
  if (capacity < 16) {
    capacity = 16;
  }
  dyn_array_t array = {
      .items = NULL, .item_size = item_size, .length = 0, .capacity = capacity};
  void *items = calloc(array.capacity, item_size);
  if (items == NULL) {
    unreachable("Unable to allocate memory for dynamic array!");
  }
  array.items = (uint8_t **)items;
  return array;
}

bool dyn_array_push(dyn_array_t *array, void *item) {
  bool has_capacity = ensure_capacity(array);
  if (!has_capacity) {
    return false;
  }
  size_t end_offset = array->length * array->item_size;
  memcpy(array->items + end_offset, item, array->item_size);
  array->length += 1;
  return true;
}

void dyn_array_pop(dyn_array_t *array, void *dest) {
  if (array->length < 1) {
    unreachable("Cannot pop from an empty array!");
  }
  pop_unchecked(array, dest);
  shrink(array);
}

void dyn_array_free(dyn_array_t *array) {
  free(array->items);
  array->items = NULL;
  array->length = 0;
  array->capacity = 0;
}
