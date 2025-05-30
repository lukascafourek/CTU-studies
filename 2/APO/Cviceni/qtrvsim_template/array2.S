  // i = s0, j = s1, c = s2, t0 = limit vnejsi smycky, t1 = limit vnitrni smycky, t3 - pomocna promenna

  addi s2, zero, 0    // c = 0;
  addi t0, zero, 500  // t0 = 500;
  addi t1, zero, 4    // t1 = 4;

  addi s0, zero, 0    // i = 0;
L1:
  addi s1, zero, 0    // j = 0;
L2:
  addi s2, s2, 1      // c++; 

  addi s1, s1, 1      // j++;
  slt  t3, s1, t1     // t3 = (s1 < t1) ? 1 : 0;
  bne  t3, zero, L2
  addi s0, s0, 1      // i++;
  slt  t3, s0, t0     // t3 = (s0 < t0) ? 1 : 0;
  bne  t3, zero, L1
