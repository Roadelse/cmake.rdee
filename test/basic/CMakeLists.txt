
cmake_minimum_required(VERSION 3.10)

project(test LANGUAGES C Fortran)


list(PREPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../../modules")

include(rdee.colorful)

# >>>>>>>>>>>>>>>>>>>>>>>>>>> check rdee.python.cmake
message(STATUS ">>>>> ${Green}start to check rdee.python.cmake${ColorReset}")
include(rdee.python)
check_python_lib(time)


# >>>>>>>>>>>>>>>>>>>>>>>>>>> check rdee.checksys.cmake
message(STATUS ">>>>> ${Green}start to check rdee.checksys.cmake${ColorReset}")
include(rdee.checksys)
message("¦ inWSL ?: ${inWSL}")
message("¦ inSW ?: ${inSW}")


# >>>>>>>>>>>>>>>>>>>>>>>>>>> check find rdee_fortran & EasyNC
message(STATUS ">>>>> ${Green}start to find rdee_fortran & EasyNC${ColorReset}")

find_Package(rdee_fortran)
find_Package(EasyNC)