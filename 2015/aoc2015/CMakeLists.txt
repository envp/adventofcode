add_library(aoc2015 OBJECT
    lib/aoc2015/error_handling.c
    lib/aoc2015/string.c
    lib/aoc2015/dynamic_array.c
)

target_include_directories(aoc2015
    PUBLIC
    include/
)

target_compile_options(aoc2015 PRIVATE
    -Wall
    -pedantic
    -Weverything
    -Wno-format-nonliteral)
