# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.27

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /Applications/CLion.app/Contents/bin/cmake/mac/aarch64/bin/cmake

# The command to remove a file.
RM = /Applications/CLion.app/Contents/bin/cmake/mac/aarch64/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release

# Include any dependencies generated for this target.
include CMakeFiles/benchmark.bin.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/benchmark.bin.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/benchmark.bin.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/benchmark.bin.dir/flags.make

CMakeFiles/benchmark.bin.dir/benchmark.cpp.o: CMakeFiles/benchmark.bin.dir/flags.make
CMakeFiles/benchmark.bin.dir/benchmark.cpp.o: /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/benchmark.cpp
CMakeFiles/benchmark.bin.dir/benchmark.cpp.o: CMakeFiles/benchmark.bin.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/benchmark.bin.dir/benchmark.cpp.o"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/benchmark.bin.dir/benchmark.cpp.o -MF CMakeFiles/benchmark.bin.dir/benchmark.cpp.o.d -o CMakeFiles/benchmark.bin.dir/benchmark.cpp.o -c /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/benchmark.cpp

CMakeFiles/benchmark.bin.dir/benchmark.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing CXX source to CMakeFiles/benchmark.bin.dir/benchmark.cpp.i"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/benchmark.cpp > CMakeFiles/benchmark.bin.dir/benchmark.cpp.i

CMakeFiles/benchmark.bin.dir/benchmark.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling CXX source to assembly CMakeFiles/benchmark.bin.dir/benchmark.cpp.s"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/benchmark.cpp -o CMakeFiles/benchmark.bin.dir/benchmark.cpp.s

CMakeFiles/benchmark.bin.dir/decryption.cpp.o: CMakeFiles/benchmark.bin.dir/flags.make
CMakeFiles/benchmark.bin.dir/decryption.cpp.o: /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/decryption.cpp
CMakeFiles/benchmark.bin.dir/decryption.cpp.o: CMakeFiles/benchmark.bin.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object CMakeFiles/benchmark.bin.dir/decryption.cpp.o"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/benchmark.bin.dir/decryption.cpp.o -MF CMakeFiles/benchmark.bin.dir/decryption.cpp.o.d -o CMakeFiles/benchmark.bin.dir/decryption.cpp.o -c /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/decryption.cpp

CMakeFiles/benchmark.bin.dir/decryption.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing CXX source to CMakeFiles/benchmark.bin.dir/decryption.cpp.i"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/decryption.cpp > CMakeFiles/benchmark.bin.dir/decryption.cpp.i

CMakeFiles/benchmark.bin.dir/decryption.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling CXX source to assembly CMakeFiles/benchmark.bin.dir/decryption.cpp.s"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/decryption.cpp -o CMakeFiles/benchmark.bin.dir/decryption.cpp.s

CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o: CMakeFiles/benchmark.bin.dir/flags.make
CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o: /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/PDVCrypt.cpp
CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o: CMakeFiles/benchmark.bin.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o -MF CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o.d -o CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o -c /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/PDVCrypt.cpp

CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing CXX source to CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.i"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/PDVCrypt.cpp > CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.i

CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling CXX source to assembly CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.s"
	/opt/homebrew/Cellar/gcc/13.2.0/bin/g++-13 $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/PDVCrypt.cpp -o CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.s

# Object files for target benchmark.bin
benchmark_bin_OBJECTS = \
"CMakeFiles/benchmark.bin.dir/benchmark.cpp.o" \
"CMakeFiles/benchmark.bin.dir/decryption.cpp.o" \
"CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o"

# External object files for target benchmark.bin
benchmark_bin_EXTERNAL_OBJECTS =

benchmark.bin: CMakeFiles/benchmark.bin.dir/benchmark.cpp.o
benchmark.bin: CMakeFiles/benchmark.bin.dir/decryption.cpp.o
benchmark.bin: CMakeFiles/benchmark.bin.dir/PDVCrypt.cpp.o
benchmark.bin: CMakeFiles/benchmark.bin.dir/build.make
benchmark.bin: /opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/libgomp.dylib
benchmark.bin: CMakeFiles/benchmark.bin.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir=/Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking CXX executable benchmark.bin"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/benchmark.bin.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/benchmark.bin.dir/build: benchmark.bin
.PHONY : CMakeFiles/benchmark.bin.dir/build

CMakeFiles/benchmark.bin.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/benchmark.bin.dir/cmake_clean.cmake
.PHONY : CMakeFiles/benchmark.bin.dir/clean

CMakeFiles/benchmark.bin.dir/depend:
	cd /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02 /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02 /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release /Users/lukascafourek/Documents/4/PDV/labs/tutorial_02/cmake-build-release/CMakeFiles/benchmark.bin.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : CMakeFiles/benchmark.bin.dir/depend

