cmake_minimum_required(VERSION 3.15)
project(01thrpool)

set(CMAKE_CXX_STANDARD 20)

# force release build
set(CMAKE_BUILD_TYPE "Release")
# enable more warnings
if(MSVC)
	add_compile_options("/W4")
else()
	add_compile_options("-Wall" "-Wextra" "-Wpedantic")
endif()

find_package(Threads REQUIRED)

add_executable(threadpool "src/main.cpp")
target_link_libraries(threadpool Threads::Threads)

