cmake_minimum_required(VERSION 3.21)

project(aoc2015 LANGUAGES C)
set(CMAKE_C_STANDARD 17)
add_compile_options(-Wall -pedantic -Wextra -fcolor-diagnostics)

add_subdirectory(aoc2015)
add_subdirectory(day01)
add_subdirectory(day02)
add_subdirectory(day03)
