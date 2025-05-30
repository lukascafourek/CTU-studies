cmake_minimum_required(VERSION 3.15)
project(pdv_lab02)

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


find_package(Threads REQUIRED)
find_package(OpenMP REQUIRED)


add_executable(02_1threads src/1threads_benchmark.cpp src/1threads.cpp)
add_executable(02_2condition_variable src/2condition_variable.cpp)

target_link_libraries(02_1threads PUBLIC OpenMP::OpenMP_CXX)
target_link_libraries(02_2condition_variable PUBLIC Threads::Threads)

# commented out, otherwise students would have to install TBB
#if(NOT MSVC)
#    # MSVC supports C++17 parallel algorithms by default
#    # GCC & Clang needs to be linked with Intel TBB, which is used to implement parallel algorithms
#    find_package(TBB REQUIRED tbb)
#    target_link_libraries(02_1threads PUBLIC TBB::tbb)
#    if(Clang AND NOT WIN32)
#        # Clang additionally needs to use stdlibc++ on non-Windows OSs instead of libc++,
#        #  because libc++ does not support parallel algorithms at all
#        target_link_libraries(02_1threads PUBLIC "-stdlib=stdlibc++")
#    endif()
#endif()
