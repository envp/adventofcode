add_executable(day03 main.c)

target_link_libraries(day03 PRIVATE aoc2015)
target_link_options(day03 PRIVATE
    -fsanitize=undefined
    -fsanitize=address
    -fno-omit-frame-pointer)

add_custom_target(runday03
    COMMAND day03 < ${CMAKE_CURRENT_LIST_DIR}/input.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
    COMMENT "Run day03 binary"
)

add_dependencies(runday03 day03)
