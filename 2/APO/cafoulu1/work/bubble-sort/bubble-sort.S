// Directives to make interesting windows visible
#pragma qtrvsim show registers
#pragma qtrvsim show memory

.option norelax

.globl array_size
.globl array_start

.text
.globl _start

_start:
    la s8, array_start
    la s1, array_size
    lw s1, 0x0(s1)
    addi s0, zero, 1
    sub s1, s1, s0
    addi s0, zero, 0

outer_loop:
    sub s3, s1, s0
    addi s2, zero, 0

inner_loop:
    slli s4, s2, 2
    add s4, s4, s8
    addi s5, s4, 4
    lw s6, 0x0(s4)
    lw s7, 0x0(s5)
    bge s7, s6, skip_swap
    sw s7, 0x0(s4)
    sw s6, 0x0(s5)

skip_swap:
    addi s2, s2, 1
    blt s2, s3, inner_loop
    addi s0, s0, 1
    blt s0, s1, outer_loop

end_loop:
    fence
    ebreak
    j end_loop

.data

array_size: 
    .word	15

array_start: 
    .word 5, 3, 4, 1, 15, 8, 9, 2, 10, 6, 11, 1, 6, 9, 12

// Specify location to show in memory window
#pragma qtrvsim focus memory array_size
