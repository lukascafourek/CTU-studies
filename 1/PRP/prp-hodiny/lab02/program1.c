#include <stdio.h>

void print_text(int n) {
    for (int i = 0; i < n; i++)
        printf("%3d. Ja jsem program 1!\n", i + 1);
}
int main(int argc, char* argv[]) {
    int n, r;
    r = scanf("%d", &n);
    if (r == 1) {
        print_text(n);
    } else if (r == 0)
        fprintf(stderr, "ERROR: Wrond input!\n");
    else
        return 2;       // ctrl + d zada prazdna hodnota a vrati to 2
    return 0;
}
