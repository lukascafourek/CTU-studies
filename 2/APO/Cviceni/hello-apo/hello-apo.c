/* Hello world example to demonstrate on multiple architectures */

/*
 * compile, run and examine for different instruction set architectures
 * build by make command where ARCH is chisen as one of
 *   native, x86, riscv, riscv64, mips, arm or aarch64
 *   make ARCH=riscv
 */

#include <stdio.h>
#include <stdint.h>

#define BINREP(PTR) print_binrep((uint8_t*)PTR, sizeof(*PTR))

void print_binrep(uint8_t *ptr, size_t len)
{
  printf("%p: ", ptr);
  for (int i = 0; i < len; i++) {
    printf("%02x ", ptr[i]);
  }
  printf("\n");
}
uint64_t v64 = 42;

int main(int argc, char *argv[])
{
    /*uint32_t v32 = 42;
    BINREP(&v32);
    BINREP(&v64);

    struct {
      int16_t a, b;
    } s = {
      .a = 42, .b = -42
    };
    BINREP(&s);*/
    float f = -0.75;
    BINREP(&f);

    union {
      float f;
      uint32_t u;
    } v = {.u = 0xc0a00000};
    printf("%f\n", v.f);

    return 0;
}
