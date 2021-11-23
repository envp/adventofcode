add_executable(day02 main.c)

target_link_libraries(day02 PRIVATE aoc2015)

add_custom_target(runday02
    COMMAND day02 < ${CMAKE_CURRENT_LIST_DIR}/input.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
    COMMENT "Run day02 binary"
)

add_dependencies(runday02 day02)
