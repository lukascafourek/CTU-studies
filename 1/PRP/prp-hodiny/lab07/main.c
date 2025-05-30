#include <stdio.h>

int main() {
    int a = 10;
    int*p = &a;
    printf("%p\n", (void*)p);
    return 0;
}
