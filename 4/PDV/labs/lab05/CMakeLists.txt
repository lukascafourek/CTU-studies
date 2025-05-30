cmake_minimum_required(VERSION 3.15)
project(pdv_lab05)

set(CMAKE_CXX_STANDARD 20)

# force release build
set(CMAKE_BUILD_TYPE "Release")
# target the instruction set of the current CPU
add_compile_options("-march=native")
# enable LTO
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
# enable more warnings
if(MSVC)
	add_compile_options("/W4")
else()
	add_compile_options("-Wall" "-Wextra" "-Wpedantic")
endif()


# OpenMP 4.0 is required for cancel constructs
find_package(OpenMP 4.0 REQUIRED)
link_libraries(OpenMP::OpenMP_CXX)


add_executable(05_1early_exit "src/1early_exit.cpp")
add_executable(05_2cancellations "src/2cancellations.cpp")
add_executable(05_3tasks "src/3tasks.cpp")
