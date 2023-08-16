

file(GLOB SRCS *.F90 *.f90 *.F *.f)
message(STATUS "srcs = ${SRCS}")

add_executable(test.x ${SRCS})

target_link_libraries(test.x rdee_fortran)

