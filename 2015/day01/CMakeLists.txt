add_executable(day01 main.c)

target_link_libraries(day01 PRIVATE aoc2015)

add_custom_target(runday01
    COMMAND day01 < ${CMAKE_CURRENT_LIST_DIR}/input.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
    COMMENT "Run day01 binary"
)

add_dependencies(runday01 day01)
