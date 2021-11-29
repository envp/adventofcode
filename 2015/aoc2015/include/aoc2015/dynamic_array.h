#ifndef AOC2015_DYNAMIC_ARRAY_H
#define AOC2015_DYNAMIC_ARRAY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// A dynamic array that stores element of identical size
typedef struct dyn_array {
  uint8_t *blocks;
  size_t item_size;
  size_t length;
  size_t capacity;
} dyn_array_t;

/// Create a dynamic array with the given initial capacity or 16 bytes,
/// whichever is higher
dyn_array_t dyn_array_init(size_t item_size, size_t capacity);

/// Push an element to the end of the dynamic array
/// Note that the size of the item being pushed must be the same
/// as the item size the array was initialized with. Anything else is UB
bool dyn_array_push(dyn_array_t *array, const void *item);

/// Remove an element from the end of the dynamic array, placing it into the
/// location provided.
void dyn_array_pop(dyn_array_t *array, void *dest);

/// Fetch an item from the array at the given index, into the address provided
/// Return if the index exists in the array or not
bool dyn_array_get(const dyn_array_t *array, size_t index, void *dest);

/// Release memory held by this dynamic array.
/// Memory is released in LIFO order
void dyn_array_free(dyn_array_t *array);

#endif /* AOC2015_DYNAMIC_ARRAY_H */