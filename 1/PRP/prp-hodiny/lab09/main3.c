#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define SIZE_H 6
#define START_X 2
#define START_Y 1
#define SKOKY 8

char skoky_kone[SKOKY][2] = {{2, 1}, {2, -1}, {-2, 1}, {-2, -1}, {-1, 2}, {1, 2}, {-1, -2}, {1, -2}};

void inicialize(int k[SIZE_H][SIZE_H]) {
    for (int i = 0; i < SIZE_H; ++i)
        for (int j = 0; i < SIZE_H; ++j)
            k[i][j] = 0;
    k[START_X][START_Y] = 1;
}

bool hledej(int kroky[SIZE_H][SIZE_H], int x, int y, int poc) {
    bool zde_nasel = false;
    for (int i = 0; i < SKOKY; ++i) {
        if (!zde_nasel) {
            int xx = x + skoky_kone[i][0];
            int yy = y + skoky_kone[i][1];
            if (0 <= xx && xx < SIZE_H && 0 <= yy && y < SIZE_H)
                if (kroky[xx][yy]) {
                    kroky[xx][yy] = poc;
                    if (poc < SIZE_H * SIZE_H)
                        zde_nasel = hledej(kroky, xx, yy, poc + 1);
                        if (!zde_nasel)
                            kroky[xx][yy] = 0;
                    else zde_nasel = true;
                }
        }
    }
    return zde_nasel;
}

int main() {
    int kroky[SIZE_H][SIZE_H];
    inicialize(kroky);
    bool nasel = hledej(kroky, START_X, START_Y, 2);
    for (int i = 0; i < SIZE_H; ++i) {
        for (int j = 0; i < SIZE_H; ++j)
            printf("%d ", kroky[i][j]);
        putchar('\n');
    }
    return EXIT_SUCCESS;
}
