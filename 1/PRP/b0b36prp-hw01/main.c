#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int ret = 0, x, y, r;
    r = scanf("%d%d", &x, &y);
    if (r == 2 && x >= -10000 && y >= -10000 && x <= 10000 && y <= 10000) {
        printf("Desitkova soustava: %d %d\n", x, y);
        printf("Sestnactkova soustava: %x %x\n", x, y);
        printf("Soucet: %d + %d = %d\n", x, y, x+y);
        printf("Rozdil: %d - %d = %d\n", x, y, x-y);
        printf("Soucin: %d * %d = %d\n", x, y, x*y);
        if (y != 0) {
            printf("Podil: %d / %d = %d\n", x, y, x/y);
        } else {
            printf("Podil: %d / %d = NaN\n", x, y);
            fprintf(stderr, "Error: Nedefinovany vysledek!\n");
            ret = 100;
        }
        printf("Prumer: %.1f\n", (float)(x+y)/2);
    } else {
        fprintf(stderr, "Error: Vstup je mimo interval!\n");
        ret = 101;
    }
    return ret;
}
