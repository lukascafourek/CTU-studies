cmake_minimum_required(VERSION 3.15)
project(pdv_lab01)

set(CMAKE_CXX_STANDARD 17)

# force release build
set(CMAKE_BUILD_TYPE "Release")
# enable more warnings
if(MSVC)
	add_compile_options("/W4")
else()
	add_compile_options("-Wall" "-Wextra" "-Wpedantic")
endif()

find_package(Threads REQUIRED)
find_package(OpenMP REQUIRED)

add_executable(01_0hello src/0hello.cpp)
add_executable(01_1memory src/1memory.cpp)
add_executable(01_2matrix src/2matrix.cpp)
add_executable(01_3false_sharing src/3false_sharing.cpp)
add_executable(01_4magic src/4magic.cpp)
add_executable(01_5decrypt src/5decrypt.cpp)

target_link_libraries(01_0hello OpenMP::OpenMP_CXX)
target_link_libraries(01_3false_sharing PUBLIC Threads::Threads)
target_link_libraries(01_4magic PUBLIC OpenMP::OpenMP_CXX)
target_link_libraries(01_5decrypt PUBLIC OpenMP::OpenMP_CXX)
