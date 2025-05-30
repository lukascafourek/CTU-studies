#include <stdio.h>
#include <stdlib.h>

size_t length(char *str) {
    size_t i = 0;
    while (str[i]) ++i;
    return i;
}

size_t rec_length(char *str) {
    if (!*str) return 0;
    else return rec_length(str + 1) + 1;
}

void recursion(int i) {
    if (i < 20)
        recursion(i + 1);
    for (int j = 0; j < i; ++j)
        putchar('*');
    putchar('\n');
}

int main() {
    int arr[3] = {1, 2, 7};
    rec_varianty(arr, 3, 2);
    return EXIT_SUCCESS;
}
