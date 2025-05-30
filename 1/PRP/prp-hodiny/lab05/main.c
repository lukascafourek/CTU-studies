#include <stdio.h>
#include <stdlib.h>

void print_array(const int* a, int n) {
    for (int i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");
}
int* create_array(int n) {
    int* a = (int*)malloc(n * sizeof(int));
    if (a == NULL) {
        fprintf(stderr, "ERROR!!!\n");
        return NULL;
    }
    for (int i = 0; i < n; ++i)
        a[i] = i + 1;
    return a;
}
int main() {
    int n;
    scanf("%d", &n);
    int* a = create_array(n);
    print_array(a, n);
    free(a);
    return 0;
}
