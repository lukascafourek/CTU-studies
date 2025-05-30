#include <stdio.h>

int main() {
    int num = 116;
    printf("%d\n", num);
    printf("%#x\n", num);
    for (int i = 7; i >= 0; i--)
        printf("%d", (num >> i) & 0x01);
    printf("\n");
    return 0;
}
