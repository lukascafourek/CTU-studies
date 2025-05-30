.globl _start

.option norelax

.text

_start:
    la s0, num1
    la s1, num2
    addi s2, zero, 0
    lw s3, 0(s0)
    lw s4, 0(s1)
    addi s5, zero, 0
    beq s4, s5, end_loop

loop1:
    add s5, s5, s3
    addi s2, s2, 1
    blt s2, s4, loop1

    la s0, num3
    la s1, num4
    lw s2, 0(s0)
    lw s3, 0(s1)
    addi s4, zero, 0
    beq s3, s4, end_loop
    add s5, zero, s3

loop2:
    bgt s5, s2, end_loop
    addi s4, s4, 1
    add s5, s5, s3
    ble s5, s2, loop2

    bgt s3, s2, L1
    add s4, zero, s2

loop3:
    sub s4, s4, s3
    blt s4, s3, end_loop
    bge s4, s3, loop3
L1:
    sub s4, s3, s2

end_loop:
    ebreak
    j end_loop
    nop

.data

num1: .word 25

num2: .word 5

num3: .word 145

num4: .word 10
