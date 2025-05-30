/*******************************************************************
  Project main function template for MicroZed based MZ_APO board
  designed by Petr Porazil at PiKRON

  change_me.c      - main file

  include your name there and license for distribution.

  Remove next text: This line should not appear in submitted
  work and project name should be change to match real application.
  If this text is there I want 10 points subtracted from final
  evaluation.

 *******************************************************************/

#define _POSIX_C_SOURCE 200112L

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>

#include "mzapo_parlcd.h"
#include "mzapo_phys.h"
#include "mzapo_regs.h"
#include "serialize_lock.h"
#include "font_types.h"

void* parlcd_base;
void* spiled_base;

union rgb {
  uint32_t d;
  struct {
    uint8_t b, g, r;
  };
};

void rgb1(union rgb color) {
  uint32_t *reg = spiled_base + SPILED_REG_LED_RGB1_o;
  *reg = color.d;
}

union knobs {
  uint32_t d;
  struct {
    uint8_t b, g, r;
    bool bp : 1;
    bool gp : 1;
    bool rp : 1;
  };
};

union knobs knobs(void) {
  uint32_t *knobs = spiled_base + SPILED_REG_KNOBS_8BIT_o;
  return (union knobs){.d = *knobs};
}

union pixel {
  uint16_t d;
  struct {
    int b : 5;
    int g : 6;
    int r : 5;
  };
};

union pixel fb[480][320];

void lcdframe(void) {
  parlcd_write_cmd(parlcd_base, 0x2c);
  for (int i = 0; i < 320 ; i++) {
    for (int j = 0; j < 480 ; j++) {
      parlcd_write_data(parlcd_base, fb[j][i].d);
    }
  }
}

#define font font_rom8x16
void lcdchar(char c, int x, int y) {
  const font_bits_t *cb = font.bits + (c * font.height);
  for (int i = 0; i < font.height; i++) {
    for (int j = 0; j < font.maxwidth; j++) {
      if (cb[i] & (0x1 << (16 - j))) {
        printf("%d:%d\n", x + j, y + i);
        fb[x + j][y + i].d = 0xffff;
      }
    }
  }
}

int main(int argc, char *argv[])
{

  /* Serialize execution of applications */

  /* Try to acquire lock the first */
  // if (serialize_lock(1) <= 0) {
  //   printf("System is occupied\n");

  //   if (1) {
  //     printf("Waitting\n");
  //     /* Wait till application holding lock releases it or exits */
  //     serialize_lock(0);
  //   }
  // }

  parlcd_base = map_phys_address(PARLCD_REG_BASE_PHYS, PARLCD_REG_SIZE, 0);
  assert(parlcd_base != NULL);
  parlcd_hx8357_init(parlcd_base);

  for (int i = 0; i < 320 ; i++) {
    for (int j = 0; j < 480 ; j++) {
      fb[j][i].d = 0x0;
    }
  }
  for (int i = 0; i < 320; i++) {
    fb[i][i].r = 0x1f;
  }

  lcdchar('A', 0, 0);
  lcdframe();

  spiled_base = map_phys_address(SPILED_REG_BASE_PHYS, SPILED_REG_SIZE, 0);
  assert(spiled_base != NULL);

  rgb1((union rgb){.r = 0x1f});

  while (true) {
    union knobs k = knobs();
    rgb1((union rgb){.r = k.r, .g = k.g, .b = k.b});
  }

  printf("Hello world\n");

  sleep(4);

  printf("Goodbye world\n");

  /* Release the lock */
  // serialize_unlock();

  return 0;
}
