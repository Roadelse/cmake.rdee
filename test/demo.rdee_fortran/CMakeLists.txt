
cmake_minimum_required(VERSION 3.10)

project(test LANGUAGES Fortran)


list(PREPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../../modules")
include(rdee.colorful)


# >>>>>>>>>>>>>>>>>>>>>>>>>>> check find EasyNC
message(STATUS ">>>>> ${Green}start to find EasyNC${ColorReset}")
find_Package(rdee_fortran)


# >>>>>>>>>>>>>>>>>>>>>>>>>>> check find EasyNC
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


SET(SRC ${CMAKE_SOURCE_DIR}/src)

add_subdirectory(src)

