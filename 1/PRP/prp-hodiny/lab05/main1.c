#include <stdio.h>
#include <string.h>

int length(const char* str) {
    int tmp = 0;
    while (str[tmp]) { tmp++; };
    return tmp;
}
int main() {
    const int M = 1000;
    char input[M];
    memset(input, 0, M);
//    fgets(input, M, stdin);
    char c;
    int idx = 0;
    char* original = "aeiouy";
    char* rewrite = "uuuaaa";
    while (scanf("%c", &c) == 1) {
        if (c == ' ' || c == '\n') {
            printf("%s %d\n", input, length(input));
            idx = 0;
            memset(input, 0, M);
            continue;
        }
        if (c >= 'A' && c <= 'Z')
            c = c - 'A' + 'a';
        else if (c >= 'a' && c <= 'z')
            c = c - 'a' + 'A';
        char* z = strchr(original, c);
        if (z != NULL)
            c = rewrite[z-original];
        input[idx++] = c;
    }
    printf("%s", input);
    return 0;
}
