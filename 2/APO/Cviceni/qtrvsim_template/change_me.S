/* template for own QtRvSim program development */

.globl array_start
.globl array_size
.globl _start
.globl __start

.option norelax

.text

__start:
_start:
	auipc s0, 0x1
	nop
	nop
	addi s0, s0, 0x400
	nop
	nop
	lw a0, 0(s0)
	auipc s0, 0x1
	nop
	nop
	addi s0, s0, 0x3e8		// 0x400 - 0x18 (1024 - 24) - 24 protoze je mezi 14. a 21. radkem 6 instrukci, tzn. 6 * 4 (velikost .word) = 24
	addi a1, zero, 1

outerloop:
	nop
	add s1, s0, zero
	nop
	nop
	add s2, s1, zero
	lw t0, 0(s1)
	add a2, a1, zero

innerloop:
	addi s1, s1, 4
	nop
	nop
	lw t1, 0(s1)
	nop
	nop
	slt t2, t0, t1
	nop
	nop
	bne t2, zero, notmin
	add t0, t1, zero
	add s2, s1, zero

notmin:
	addi a2, a2, 1
	nop
	nop
	bne a0, a2, innerloop

	lw t1, 0(s0)
	sw t0, 0(s0)
	nop
	sw t1, 0(s2)
	addi s0, s0, 4
	addi a1, a1, 1
	nop
	nop
	bne a0, a1, outerloop

loop:
    fence
    ebreak
    j loop

.org 0x400

.data

array_size:
.word	15
array_start:
.word	5, 3, 4, 1, 15, 8, 9, 2, 10, 6, 11, 1, 6, 9, 12
