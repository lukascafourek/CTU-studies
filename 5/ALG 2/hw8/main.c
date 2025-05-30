#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

int main(int argc, char **argv) {
    int r, N, DH, H1, H2;
    if ((r = scanf("%d %d %d %d\n", &N, &DH, &H1, &H2)) != 4) {
        fprintf(stderr, "Error: Chybny vstup!\n");
        return 1;
    }
    int **arr = (int**)malloc(N * sizeof(int*));
    int **atr = (int**)malloc(N * sizeof(int*));
    if (!arr || !atr) {
        fprintf(stderr, "ERROR while allocating memory for array!\n");
        return 1;
    }
    for (size_t i = 0; i < N; i++) {
        arr[i] = (int*)malloc(N * sizeof(int));
        atr[i] = (int*)malloc(N * sizeof(int));
        if (!arr[i] || !atr[i]) {
            fprintf(stderr, "ERROR while allocating memory for array row!\n");
            return 1;
        }
        for (size_t j = 0; j < N; j++) {
            arr[i][j] = 0;
            atr[i][j] = 0;
        }
    }
    int *num = (int*)malloc(8000 * sizeof(int));
    int *best_atr = (int*)malloc(8000 * sizeof(int));
    if (!num || !best_atr) {
        fprintf(stderr, "ERROR while allocating memory for array!\n");
        return 1;
    }
    for (size_t i = 0; i < 8000; i++) {
        num[i] = 0;
        best_atr[i] = INT_MIN;
    }
    int tmp, max_num = 0;
    for (int i = 0; i < N; i++) {
        if ((r = scanf("%d", &arr[i][0])) != 1) {
            fprintf(stderr, "Error: Chybny vstup!\n");
            return 1;
        }
        tmp = arr[i][0];
        num[tmp] += 1;
        for (int j = 1; j < N; j++) {
            if ((r = scanf(" %d", &arr[i][j])) != 1) {
                fprintf(stderr, "Error: Chybny vstup!\n");
                return 1;
            }
            tmp = arr[i][j];
            num[tmp] += 1;
        }
    }
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            if (i == 0 && j == 0) {
                atr[i][j] = 0;
                tmp = arr[i][j];
                best_atr[tmp] = atr[i][j];
            } else if (j == 0) {
                atr[i][j] = atr[i-1][j];
                for (int k = j; k < N; k++) {
                    atr[i][j] -= arr[i-1][k];
                }
                int x = arr[i][j] - DH;
                int y = arr[i-1][j] - DH;
                if (y < 0) {
                    y = 0;
                }
                if (x > y) {
                    for (int k = y; k < x; k++) {
                        atr[i][j] += 2 * num[k] * k;
                    }
                }
                tmp = arr[i][j];
                if (atr[i][j] > best_atr[tmp]) {
                    best_atr[tmp] = atr[i][j];
                }
            } else {
                atr[i][j] = atr[i][j-1];
                for (int k = i; k < N; k++) {
                    atr[i][j] -= arr[k][j-1];
                }
                int x = arr[i][j] - DH;
                int y = arr[i][j-1] - DH;
                if (y < 0) {
                    y = 0;
                }
                if (x > y) {
                    for (int k = y; k < x; k++) {
                        atr[i][j] += 2 * num[k] * k;
                    }
                }
                tmp = arr[i][j];
                if (atr[i][j] > best_atr[tmp]) {
                    best_atr[tmp] = atr[i][j];
                }
            }
            if (i == N - 1 && j == N - 1) {
                max_num = arr[i][j];
            }
        }
    }
    int max_atr = 0, max = 0, tmp3 = max_num, tmp4 = tmp3 - H2;
    tmp = num[tmp3];
    for (int i = max_num; i >= 0; i--) {
        if (tmp > 0 && tmp3 >= 0) {
            max_atr += best_atr[tmp3];
            max += 1;
            tmp4 = tmp3 - H2;
            tmp3 = tmp3 - H1;
            tmp = num[tmp3];
        } else if (tmp3 > tmp4) {
            tmp3 -= 1;
            tmp = num[tmp3];
        } else {
            break;
        }
    }
    printf("%d %d\n", max, max_atr);
    for (size_t i = 0; i < N; i++) {
        free(arr[i]);
        free(atr[i]);
    }
    free(arr);
    free(atr);
    free(num);
    free(best_atr);
    return 0;
}
