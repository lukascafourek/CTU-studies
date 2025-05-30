#include <stdio.h>
#include <stdlib.h>

int main() {
    int N, K, L, S;
    scanf("%d %d %d %d", &N, &K, &L, &S);

    int** grid;
    grid = (int**)malloc(N * sizeof(int*));
    if (grid == NULL) {
        fprintf(stderr, "Failed to allocate memory for 'grid'.\n");
        exit(EXIT_FAILURE);
    }
    int** prefixForest;
    prefixForest = (int**)malloc((N+1) * sizeof(int*));
    if (grid == NULL) {
        fprintf(stderr, "Failed to allocate memory for 'prefixForest'.\n");
        exit(EXIT_FAILURE);
    }
    int** prefixRock;
    prefixRock = (int**)malloc((N+1) * sizeof(int*));
    if (grid == NULL) {
        fprintf(stderr, "Failed to allocate memory for 'prefixRock'.\n");
        exit(EXIT_FAILURE);
    }
    for(int i = 0; i < N+1; i++) {
        if (i < N) {
            grid[i] = (int*)malloc(N * sizeof(int));
        }
        prefixForest[i] = (int*)malloc((N+1) * sizeof(int));
        prefixRock[i] = (int*)malloc((N+1) * sizeof(int));
        prefixForest[i][0] = prefixForest[0][i] = prefixRock[i][0] = prefixRock[0][i] = 0;
    }

    for (int i = 1; i <= N; i++) {
        for (int j = 1; j <= N; j++) {
            scanf("%d", &grid[i-1][j-1]);
            prefixForest[i][j] = prefixForest[i-1][j] + prefixForest[i][j-1] - prefixForest[i-1][j-1] + (grid[i-1][j-1] == 1);
            prefixRock[i][j] = prefixRock[i-1][j] + prefixRock[i][j-1] - prefixRock[i-1][j-1] + (grid[i-1][j-1] == 2);
        }
    }

    int maxForest = 0;

    for (int i = 0; i <= N - K; i++) {
        for (int j = 0; j <= N - K; j++) {
            int totalForest = prefixForest[i + K][j + K] - prefixForest[i][j + K] - prefixForest[i + K][j] + prefixForest[i][j];
            int centerRock = prefixRock[i + K - L][j + K - L] - prefixRock[i + L][j + K - L] - prefixRock[i + K - L][j + L] + prefixRock[i + L][j + L];
            
            if (centerRock >= S && totalForest > maxForest) {
                maxForest = totalForest;
            }
        }
    }

    printf("%d\n", maxForest);

    for(int i = 0; i < N+1; i++) {
        if (i < N) {
            free(grid[i]);
        }
        free(prefixForest[i]);
        free(prefixRock[i]);
    }
    free(grid);
    free(prefixForest);
    free(prefixRock);

    return 0;
}
