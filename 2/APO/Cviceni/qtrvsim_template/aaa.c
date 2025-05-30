#include <stdint.h>

#define SPILED_REG_BASE    0xffffc100

#define SPILED_REG_LED_LINE_o           0x004
#define SPILED_REG_LED_RGB1_o           0x010
#define SPILED_REG_LED_RGB2_o           0x014
#define SPILED_REG_LED_KBDWR_DIRECT_o   0x018

#define SPILED_REG_KBDRD_KNOBS_DIRECT_o 0x020
#define SPILED_REG_KNOBS_8BIT_o         0x024

//

#define SERIAL_PORT_BASE   0xffffc000

#define SERP_RX_ST_REG_o           0x00
#define SERP_RX_ST_REG_READY_m      0x1
#define SERP_RX_ST_REG_IE_m         0x2

#define SERP_RX_DATA_REG_o         0x04

#define SERP_TX_ST_REG_o           0x08
#define SERP_TX_ST_REG_READY_m      0x1
#define SERP_TX_ST_REG_IE_m         0x2

#define SERP_TX_DATA_REG_o         0x0c

// int gready(void) {
//     char *status = (char*)(SERIAL_PORT_BASE + SERP_RX_ST_REG_o);
//     return *status & SERP_RX_ST_REG_READY_m;
// }
// char gchar(void) {
//     char *data = (char*)(SERIAL_PORT_BASE + SERP_RX_ST_REG_o);
//     while (!gready());
//     return *data;
// }
// int pready(void) {
//     char *status = (char*)(SERIAL_PORT_BASE + SERP_TX_ST_REG_o);
//     return *status & SERP_TX_ST_REG_READY_m;
// }
// void pchar(char c) {
//     char *data = (char*)(SERIAL_PORT_BASE + SERP_TX_ST_REG_o);
//     while (!pready());
//     *data = c;
// }

#define LCD_FB_START       0xffe00000
#define LCD_FB_END         0xffe4afff
#define LCD_WIDTH          480
#define LCD_HEIGHT         320

union pixel {
    uint16_t d;
    struct {
        int b : 5;
        int g : 6;
        int r : 5;
    };
};

void lcd(int x, int y, union pixel color) {
    uint16_t *p = (uint16_t*)(LCD_FB_START + (2 * (y * LCD_WIDTH + x)));
    *p = color.d;
}

int main(int argc, char *argv[]) {
    union pixel p = {.d = 0};
    for (int i = 0; i < LCD_HEIGHT; i++) {
        lcd(i + (LCD_WIDTH - LCD_HEIGHT) / 2, i, p);
        lcd(i + (LCD_WIDTH - LCD_HEIGHT) / 2, LCD_HEIGHT - i, p);
        p.r++;
    }
    return 0;
}
