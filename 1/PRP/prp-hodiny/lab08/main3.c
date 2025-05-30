#include <stdio.h>
#include <stdlib.h>

#define SIZE 10

int main() {
    int delky[SIZE] = {3, 4, 5, 2, 4, 5, 6, 7, 2, 4};
    double** Frantiska = (double**)malloc(sizeof(double*) * SIZE);
    if (Frantiska) {
        for (int i = 0; i < SIZE; ++i){
            Frantiska[0] = (double*)malloc(sizeof(double) * (delky[i] + 1));
            if (Frantiska[i]){
                Frantiska[i][0]= delky[i];
                for (int j = 1; j < delky[i]; ++j)
                    scanf("%lf", &Frantiska[i][j]);
            }
        }
    }
}
