// apo-sort.S file template, rename and implement the algorithm
// Test algorithm in qtrvsim_gui program
// Select the CPU core configuration with delay-slot
// This setups requires (for simplicity) one NOP instruction after
// each branch and jump instruction (more during lecture about pipelining)
// The code will be compiled and tested by external riscv64-unknown-elf-gcc
// compiler by teachers, you can try make in addition, but testing
// by internal assembler should be enough

// copy directory with the project to your repository to
// the directory work/apo-sort
// critical is location of the file work/apo-sort/apo-sort.S
// and cache parameters work/apo-sort/d-cache.par
// which is checked by the scripts

// The file d-cache.par specifies D cache parameters in the form
//   <policy>,<#sets>,<#words in block>,<#ways>,<write method>
// The example is
//   lru,1,1,1,wb
// The cache size is limited to 16 words maximum.

// Directives to make interesting windows visible
#pragma qtrvsim show registers
#pragma qtrvsim show memory

.option norelax

.globl    array_size
.globl    array_start

.text
.globl _start

_start:
	la s0, array_size
	lw a0, 0(s0)
	la s0, array_start
	addi a1, zero, 1

outerloop:
	add s1, s0, zero
	add s2, s1, zero
	lw t0, 0(s1)
	add a2, a1, zero

innerloop:
	addi s1, s1, 4
	lw t1, 0(s1)
	slt t2, t0, t1
	bne t2, zero, notmin
	add t0, t1, zero
	add s2, s1, zero

notmin:
	addi a2, a2, 1
	bne a0, a2, innerloop

	lw t1, 0(s0)
    nop
	sw t0, 0(s0)
	sw t1, 0(s2)
	addi s0, s0, 4
	addi a1, a1, 1
	bne a0, a1, outerloop

//Final infinite loop
end_loop:
	fence           // flush cache memory
	ebreak          // stop the simulator
	j end_loop


.data
// .align    2 // not supported by qtrvsim yet

array_size:
.word	15
array_start:
.word	5, 3, 4, 1, 15, 8, 9, 2, 10, 6, 11, 1, 6, 9, 12

// Specify location to show in memory window
#pragma qtrvsim focus memory array_size
