add_library(aoc2015 OBJECT
    lib/aoc2015/error_handling.c
    lib/aoc2015/string.c
)

target_include_directories(aoc2015
    PUBLIC
    include/
)
