#include <stdio.h>
#include <stdlib.h>

#define EXIT_SUCCESS 0
#define ERROR_INPUT 100
#define MAX_NUMBER 1000000

int read_input(long long input, int r, int ret);
void print_error(void);
void decompose(long long input, int* pn_table);
int sieve_of_eratosthenes(int* pn_table);

int main(int argc, char *argv[]) {
    long long input;
    int r, ret = EXIT_SUCCESS;
    int* pn_table = malloc((MAX_NUMBER + 1) * sizeof(int));
    sieve_of_eratosthenes(pn_table);
    while (1) {
        r = scanf("%lld ", &input);
        if (input == 0) {
            break;
        }
        ret = read_input(input, r, ret);
        if (ret == EXIT_SUCCESS) {
            decompose(input, pn_table);
        }
        else {
            print_error();
            free(pn_table);
            return ret;
        }
    }
    free(pn_table);
    return ret;
}
int read_input(long long input, int r, int ret) {    // this reads input
    if (r != 1 || input < 0) {
        ret = ERROR_INPUT;
    }
    return ret;
}
void print_error(void) {    // this prints error
    fprintf(stderr, "Error: Chybny vstup!\n");
}
// this below precalculates table of prime numbers via Sieve of Eratosthenes
int sieve_of_eratosthenes(int* pn_table) {
    for (int p = 2; p <= MAX_NUMBER; p++) {
        pn_table[p] = p;
    }
    for (int p = 2; p * p <= MAX_NUMBER; p++) {
        if (pn_table[p] != 0) {
            for (int i = 2; i < MAX_NUMBER; i++) {
                if (pn_table[p]*i > MAX_NUMBER) {
                    break;
                }
                else {
                    pn_table[pn_table[p]*i] = 0;
                }
            }
        }
    }
    int i = 0;
    for (int p = 2; p < MAX_NUMBER; p++)
        if (pn_table[p] != 0) {
            pn_table[i] = p;
            i++;
        }
    return *pn_table;
}
// this below decomposes the given inputs into the product of prime numbers
void decompose(long long input, int* pn_table) {
    printf("Prvociselny rozklad cisla %lld je:\n", input);
    if (input == 1) {
        printf("%lld", input);
    }
    int i = 0;
    while (input > 1) {
        int j = 0, n = pn_table[i];
        while (input % n == 0) {
            j++;
            input /= n;
        }
        if (j != 0) {
            printf("%d", n);
            if (j != 1) {
                printf("^%d", j);
            }
            if (input != 1) {
                printf(" x ");
            }
        }
        i++;
    }
    putchar('\n');
}
