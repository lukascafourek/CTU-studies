# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release

# Include any dependencies generated for this target.
include CMakeFiles/07_1autovectorization.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/07_1autovectorization.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/07_1autovectorization.dir/flags.make

CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.o: CMakeFiles/07_1autovectorization.dir/flags.make
CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.o: ../src/1autovectorization.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.o -c /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/src/1autovectorization.cpp

CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/src/1autovectorization.cpp > CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.i

CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/src/1autovectorization.cpp -o CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.s

# Object files for target 07_1autovectorization
07_1autovectorization_OBJECTS = \
"CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.o"

# External object files for target 07_1autovectorization
07_1autovectorization_EXTERNAL_OBJECTS =

07_1autovectorization: CMakeFiles/07_1autovectorization.dir/src/1autovectorization.cpp.o
07_1autovectorization: CMakeFiles/07_1autovectorization.dir/build.make
07_1autovectorization: CMakeFiles/07_1autovectorization.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable 07_1autovectorization"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/07_1autovectorization.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/07_1autovectorization.dir/build: 07_1autovectorization

.PHONY : CMakeFiles/07_1autovectorization.dir/build

CMakeFiles/07_1autovectorization.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/07_1autovectorization.dir/cmake_clean.cmake
.PHONY : CMakeFiles/07_1autovectorization.dir/clean

CMakeFiles/07_1autovectorization.dir/depend:
	cd /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release /mnt/c/Users/lukas/Documents/PDV/labs/lab07_solution/cmake-build-release/CMakeFiles/07_1autovectorization.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/07_1autovectorization.dir/depend

