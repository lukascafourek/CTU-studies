Homework 4 example with code analysis
=====================================

Homework 4 Assignment
=====================

Files program-x86.list and program-mips.list are dump/listing
of a program obtained from a computer memory.

Program has been compiled using C compiler for x86 32-bit
computer and OS GNU/Linux first. Then the same code has
been compiled for big-endian variant
of MIPS32 architecture.

Program consists of a function called toplevel_fnc(), which calls
a function subroutine_fnc(). One or both functions calls operating
systems services.

Your assignment is to analyze/decode program function.

The file syscalls.lst includes listing of some system calls.
Some small subset of these is used by the analyzed program.
Edit list such way, that only entries corresponding to actually
used systemcalls for your program variant are present
in the file. Keep formatting, write exactly single systemcall
per single line, use no separators or additional spaces
(the list matching program 1 point)

Fill list of the subroutine_fnc() function arguments with
types in header file subroutine.h.
The parameters are typically integer numbers (int), characters
(char) or pointers to these types.
(correct parameter types and count 2 points)

Copy parameters with types to a file subroutine.c and implement
body of the function subroutine_fnc(). The C implementation
of subroutine_fnc() should process parameters and call systemcalls
same way as original assembly code.

The remaining algorithm parts required to fulfil equivalent
operation as original code should be implemented in toplevel.c
file as body of toplevel_fnc(). A value returned by whole program
should be the same as is returned by original code.

It is required that submitted program(source code) can be compiled
by C language compiler. This requirement is equivalent
to calling make in the directory. Do not modify targets
and rules defined by provided Makefile.

README.txt - this file
program-mips.list - dump of computer/process memory for MIPS
                   architecture variant,
                   you can add notes, comments etc in this file
program-x86.list - dump of computer/process memory for x86
                   architecture variant,
                   you can add notes, comments etc in this file
Makefile - instructions for the program build by make utility
main.c - program entry point and main() function which calls
                   toplevel_fnc(), do not edit the file
subroutine.h - declaration of function subroutine_fnc(),
                   fill in arguments with types and return value type
subroutine.c - definition, implementation of the subroutine_fnc()
                   function, edit/implement body
toplevel.c - program top level logic/algorithm implementation, edit
syscalls.lst - list of the syscalls used in given program sample,
                   edit to not include calls not used in given fragment

Additional files in the example task
====================================

The files used to generate the task assignment

testbench.h - test bench defines to allow inline system calls
              to make task assignment straightforward
testbench.c - test bench main which invokes toplevel_fnc(),
              prints banners and subroutine return value
task-10.c   - actual file with task
              INTERNAL_SYSCALL(<name>, err_res, <arg_num>, <arguments>);
	      is used to ensure direct inlining of system call

Binary files with task program

program-mips - code compiled for MIPS
program-x86  - code compiled for x86 in 32-bit mode

Appropriate objdump or mips-elf-objdump can be used to analyze binaries

objdump --section=my_text --disassemble --show-raw-insn program-x86
objdump --section=my_data --full-contents program-x86

the results are program-mips.list and program-x86.list files.

Listing with code interleaved with C sources can be obtained

objdump --section=my_text --source --show-raw-insn program-x86
objdump --section=my_data --full-contents program-x86

the results are files program-mips.src and program-x86.src.

Solution
========

The analyzed code listing for x86, MIPS with comments
in Czech and English language

program-mips.src.commented
program-mips.src.komentovany
program-x86.list.commented
program-x86.list.komentovany

The source files files reconstructed for submission to the test system

subroutine.c
subroutine.h
toplevel.c

The files including solution for submission for manual control

du4-x86+mips-example-en.pdf
du4-x86+mips-example.pdf
du4-x86+mips-solved-en.pdf
du4-x86+mips-solved.pdf
