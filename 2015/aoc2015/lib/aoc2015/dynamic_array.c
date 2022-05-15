#include "aoc2015/dynamic_array.h"
#include "aoc2015/error_handling.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

const size_t DYN_ARRAY_DEFAULT_CAPACITY = 16;

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
    return DYN_ARRAY_DEFAULT_CAPACITY;
  }
  return (current / 3) * 2;
}

/// Ensure the given array has enough capacity to insert the next elem
/// This will trigger an O(n) re-allocation every time the array length reaches
/// a certain fraction of its capacity
static bool ensure_capacity(dyn_array_t *array) {
  if (array->length == array->capacity) {
    const size_t new_capacity = next_size(array->capacity);
    array->blocks = realloc(array->blocks, new_capacity * array->item_size);
    /// Clear the new allocated memory
    if (errno != ENOMEM) {
      size_t num_allocated = new_capacity - array->capacity;
      memset(array->blocks + array->capacity, 0,
             array->item_size * num_allocated);
    }
    // realloc(...) doesn't change the array upon failure, but does set errno
    // https://linux.die.net/man/3/realloc
    return errno != ENOMEM;
  }
  return true;
}

/// Read the element located at position 'index'
inline static void get_elem_unchecked(const dyn_array_t *array, size_t idx,
                                      void *dest) {
  size_t offset = idx * array->item_size;
  memcpy(dest, &array->blocks[offset], array->item_size);
}

inline static void set_elem_unchecked(dyn_array_t *array, size_t idx,
                                      const void *const src) {
  size_t offset = idx * array->item_size;
  memcpy(&array->blocks[offset], src, array->item_size);
}

/// Copy the last element of the array into the destination, and clear it
inline static void pop_unchecked(dyn_array_t *array, void *dest) {
  get_elem_unchecked(array, array->length - 1, dest);
  size_t offset = (array->length - 1) * array->item_size;
  memset(&array->blocks[offset], 0, array->item_size);
}

/// Shrink the array if the length under a certain fraction of the capacity
inline static void shrink(dyn_array_t *array) {
  size_t threshold = prev_size(array->capacity);
  if (array->length >= threshold) {
    return;
  }
  array->blocks = realloc(array->blocks, threshold * array->item_size);
  if (errno == ENOMEM) {
    unreachable("Unable to de-allocate memory?!");
  }
}

dyn_array_t dyn_array_init(size_t item_size, size_t capacity) {
  if (capacity < DYN_ARRAY_DEFAULT_CAPACITY) {
    capacity = DYN_ARRAY_DEFAULT_CAPACITY;
  }
  dyn_array_t array = {.blocks = NULL,
                       .item_size = item_size,
                       .length = 0,
                       .capacity = capacity};
  void *items = calloc(array.capacity, item_size);
  if (items == NULL) {
    unreachable("Unable to allocate memory for dynamic array!");
  }
  array.blocks = (uint8_t *)items;
  return array;
}

bool dyn_array_push(dyn_array_t *array, const void *const item) {
  bool has_capacity = ensure_capacity(array);
  if (!has_capacity) {
    return false;
  }
  size_t end_offset = array->length * array->item_size;
  memcpy(&array->blocks[end_offset], item, array->item_size);
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

bool dyn_array_get(const dyn_array_t *array, size_t index, void *dest) {
  if (index >= array->length) {
    return false;
  }
  get_elem_unchecked(array, index, dest);
  return true;
}

bool dyn_array_set(dyn_array_t *array, size_t index, const void *const src) {
  if (index >= array->length) {
    return false;
  }
  set_elem_unchecked(array, index, src);
  return true;
}

bool dyn_array_resize(dyn_array_t *array, size_t minimum_length) {
  if (minimum_length > array->capacity) {
    size_t next_capacity = array->capacity;
    while (next_capacity < minimum_length) {
      next_capacity = next_size(next_capacity);
    }
    array->blocks = realloc(array->blocks, next_capacity * array->item_size);
    if (errno != ENOMEM) {
      size_t num_allocated = next_capacity - array->capacity;
      memset(array->blocks + array->capacity, 0,
             array->item_size * num_allocated);
    }
    array->capacity = next_capacity;
    return errno != ENOMEM;
  }
  return false;
}

bool dyn_array_fill(dyn_array_t *array, const void *const item, size_t count) {
  dyn_array_resize(array, count);
  // TODO: What is a good way to handle ENOMEM here?
  if (errno == ENOMEM) {
    return false;
  }
  for (size_t idx = 0; idx != count; ++idx) {
    set_elem_unchecked(array, idx, item);
  }
  if (count > array->length) {
    array->length = count;
  }
  return true;
}

void dyn_array_free(dyn_array_t *array) {
  free(array->blocks);
  array->blocks = NULL;
  array->length = 0;
  array->capacity = 0;
}
