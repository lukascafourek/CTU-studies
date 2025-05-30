#include <stdio.h>
#include <stdbool.h>

#define SIZE_H 6

void inicializace(char *d, bool *r, bool *d1, bool *d2) {
    for (int i = 0; i < SIZE_H; ++i) {
        d[i] = 0;
        r[i] = true;
    }
    for (int i = 0; i < 2 * SIZE_H; ++i) {
        d1[i] = true;
        d2[i] = true;
    }
}

void find(char *d, bool *r, bool *d1, bool *d2, int i) {
    for (int j = 0; j < SIZE_H; ++j) {
        if (r[j] && d1[i + j] && d2[i - j + SIZE_H]) {
            d[i] = j;
            r[j] = false; d1[i + j] = false; d2[i - j + SIZE_H] = false;
            if (i < SIZE_H - 1) find(d, r, d1, d2, i + 1);
            else {
                for (int k = 0; k < SIZE_H; ++k) printf("%d ", d[k]);
                putchar('\n');
            }
            r[j] = true; d1[i + j] = true; d2[i - j + SIZE_H] = true;
        }
    }
}

int main() {
    char damy[SIZE_H];
    bool radek[SIZE_H], diag1[2 * SIZE_H - 1], diag2[2 * SIZE_H - 1];
    inicializace(damy, radek, diag1, diag2);
    find(damy, radek, diag1, diag2, 0);
    return 0;
}
