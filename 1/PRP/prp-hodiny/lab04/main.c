#include <stdio.h>
#include <stdlib.h>

#define ERROR_INPUT 100
#define ERROR_RANGE 101
#define MIN_VALUE 1
#ifndef MAX_VALUE
#define MAX_VALUE 10
#endif
#define OPPOSITE(a) (-1) * a
#define OPPOSITE2(a) ~a | 0x01
#define OPPOSITE3(a) a == 1 ? -1 : 1

int read_input(int *n);
void print_error(int ret, int n);
void print_line(int k);
void print_triangle(int n);

int main() {
    int n;
    int ret = read_input(&n);
    if (ret == EXIT_SUCCESS) {
        print_triangle(n);
    } else {
    print_error(ret, n);
    }
    int num = 1;
    for (int i = 0; i < 1000; ++i) {
        printf("%d ", num);
        num = OPPOSITE2(num);
    }
    return ret;
}
void print_error(int ret, int n) {
    switch (ret) {
    case ERROR_INPUT:
        fprintf(stderr, "ERROR: Cannot read integer value from the standard input.\n");
        break;
    case ERROR_RANGE:
        fprintf(stderr, "ERROR: Given value %d is not within range [%d, %d].\n", 
                n, MIN_VALUE, MAX_VALUE);
        break;
    }
}
int read_input(int *n) {
    int r, ret = EXIT_SUCCESS;
    printf("Enter number of lines in the range %d to %d: ", MIN_VALUE, MAX_VALUE);
    r = scanf("%d", &n);
    if (r != 1) {
        ret = ERROR_INPUT;
    } else if (n < MIN_VALUE || n > MAX_VALUE) {
        ret = ERROR_RANGE;
    }
}
void print_line(int k) {
    for (int j = 0; j < k; ++j)
        putchar('*');
    putchar('\n');
}
void print_triangle(int n) {
        putchar('\n');
        for (int i = 0; i < n; ++i) {
            print_line(n - i);
        }
}
