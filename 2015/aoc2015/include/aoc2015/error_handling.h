#ifndef AOC2015_ERROR_HANDLING_H
#define AOC2015_ERROR_HANDLING_H

#define NORETURN __attribute__((noreturn))

/// Use to indicate that a line is unreachable
/// Prints an error message and exits
NORETURN void unreachable(const char *restrict fmt, ...);

#endif /* AOC2015_ERROR_HANDLING_H */
