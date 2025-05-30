#include <stdio.h>
#include <stdlib.h>

int** allocate_memory(int size) {
    int** array = (int**)malloc(size * sizeof(int*));
    if (!array) {
        fprintf(stderr, "ERROR while allocating memory for array!\n");
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < size; i++) {
        array[i] = (int*)malloc(size * sizeof(int));
        if (!array[i]) {
            fprintf(stderr, "ERROR while allocating memory for array row!\n");
            exit(EXIT_FAILURE);
        }
        for (size_t j = 0; j < size; j++) {
            array[i][j] = 0;
        }
    }
    return array;
}

void free_memory(int** array, int size) {
    for (size_t i = 0; i < size; i++) {
        free(array[i]);
    }
    free(array);
}

void read_input(int n, int** forestPrefix, int** rockPrefix) {
    int input = 0;
    for (size_t i = 1; i <= n; i++) {
        for (size_t j = 1; j <= n; j++) {
            scanf("%d", &input);
            forestPrefix[i][j] = forestPrefix[i-1][j] + forestPrefix[i][j-1] - forestPrefix[i-1][j-1] + (input == 1);
            rockPrefix[i][j] = rockPrefix[i-1][j] + rockPrefix[i][j-1] - rockPrefix[i-1][j-1] + (input == 2);
        }
    }
}

int findMax(int n, int k, int l, int s, int** forestPrefix, int** rockPrefix) {
    int max = 0;
    for (size_t i = 0; i <= n - k; i++) {
        for (size_t j = 0; j <= n - k; j++) {
            int forests = forestPrefix[i + k][j + k] - forestPrefix[i][j + k] - forestPrefix[i + k][j] + forestPrefix[i][j];
            int rocks = rockPrefix[i + k - l][j + k - l] - rockPrefix[i + l][j + k - l] - rockPrefix[i + k - l][j + l] + rockPrefix[i + l][j + l];
            if (rocks >= s && forests > max) {
                max = forests;
            }
        }
    }
    return max;
}

int main(int argc, char** argv) {
    int n, k, l, s;
    if (scanf("%d %d %d %d", &n, &k, &l, &s) != 4) {
        fprintf(stderr, "ERROR: wrong input!\n");
        exit(EXIT_FAILURE);
    }
    int** forestPrefix = allocate_memory(n + 1);
    int** rockPrefix = allocate_memory(n + 1);
    read_input(n, forestPrefix, rockPrefix);
    int max = findMax(n, k, l, s, forestPrefix, rockPrefix);
    printf("%d\n", max);
    free_memory(forestPrefix, n + 1);
    free_memory(rockPrefix, n + 1);
    return EXIT_SUCCESS;
}
