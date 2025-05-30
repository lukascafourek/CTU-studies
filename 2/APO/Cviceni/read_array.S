.option norelax

.globl pole
.globl _start

.text

_start:

la   s0, pole       // adresa zacateku pole do registru s0
addi s1, x0, 3      // Pocet pruchodu cyklem (pocet iteraci)
nop                  // Proc je zde nop? Zkuste jej odranit.. Zmeni se hit rate?

loop:
    beq s1, x0, konec
    nop
    lw s2, 0(s0)      // Cteni 0-teho prvku pole
    lw s2, 4(s0)      // Cteni 1. prvku (4/4=1)

    lw s2, 36(s0)     // Cteni 9. prvku (36/4=9)
    lw s2, 40(s0)     // Cteni 10.prvku (40/4=10)
   
    addi s1, s1, -1
    j loop
    nop
konec:
nop  

end_loop:           //Koncova nekonecna smycka
    fence            // flush cache memory
    ebreak           // stop the simulator    
    j end_loop
    nop

.org 0x2000

.data

pole:
.word    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12