#include <stdio.h>
#include <stdlib.h>

#ifndef MAX_N
    #define MAX_N 5
#endif

#ifndef FILENAME
    #define FILENAME "binary_file_with_numbers.bin"
#endif

struct test {
    char znak;
    int ival;
    char* cval;
};
typedef struct test test_t;

/**
 * Reads from STDIN and returs array with the read interger numbers.
*/
int* read_from_stdin(int n) {
    int num = 0;
    int r = -1;

    int *nums = (int*) malloc (sizeof(int) * n);

    printf("Enter %d numbers: ", n);
    for (int i = 0; i < n; i++) {
        r = scanf("%d", &num);
        if (r != 1) {
            break;
        }
        nums[i] = num;
    }   
    return nums;
}

/**
 * Frees given array.
*/
void free_memory(int *arr) {
    free(arr);
}

/**
 * Writes content of the array `arr` to binary file `filename`.
*/
void write_to_bin(int *arr, int n, const char *filename);

/**
 * Reads content of the binary file `filename` and prints it to STDOUT.
*/
void read_from_bin(const char *filename, int n);

/**
 * Writes content of a struct to a FILENAME.
*/
void write_struct_to_bin();

/**
 * Reads content of FILENAME and prints it to STDOUT.
*/
void read_struct_from_bin();

int le_to_be(int x);

int main(int argc, char const *argv[])
{
    /*int *numbers = read_from_stdin(MAX_N);
    write_to_bin(numbers, MAX_N, FILENAME);
    read_from_bin(FILENAME, MAX_N);
    free_memory(numbers);*/

    printf("%d\n", le_to_be(1));
    write_struct_to_bin();
    read_struct_from_bin();

    return 0;
}
void write_to_bin(int *arr, int n, const char *filename) {
    FILE *f = fopen(filename, "wb");
    if (f == NULL) {
        fprintf(stderr, "Nepovedlo se!!!\n");
        return;
    }
    size_t r = fwrite(arr, sizeof(int), n, f);
    if (r != n) {
        fprintf(stderr, "Cislo se nepovedlo zapsat!!!\n");
        return;
    }
    if (fclose(f) == EOF) {
        fprintf(stderr, "Soubor se nepovedlo zavrit!!!\n");
        return;
    }
}
void read_from_bin(const char *filename, int n) {
    FILE *f = fopen(filename, "rb");
    if (f == NULL) {
        fprintf(stderr, "Nepovedlo se!!!\n");
        return;
    }
    int* a = (int*)malloc(sizeof(int) * 2 * n);
    size_t r = fread(a, sizeof(int), 2 * n, f);
    if (r != 2 * n) {
        fprintf(stderr, "Povedlo se mi nacist %zu cisel\n", r);
        return;
    }
    for (size_t i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");
    if (fclose(f) == EOF) {
        fprintf(stderr, "Soubor se nepovedlo zavrit!!!\n");
        return;
    }
}
void write_struct_to_bin() {
    FILE *f = fopen("input.bin", "wb");
    if (f == NULL) {
        fprintf(stderr, "Nepovedlo se!!!\n");
        return;
    }
    test_t t;
    for (int i = 0; i < 30; ++i) {
        t.ival = le_to_be(i + 1);
        t.cval = NULL;
        t.znak = 'a' + i;
        size_t r = fwrite(&t.znak, sizeof(char), 1, f);
        r = fwrite(&t.ival, sizeof(int), 1, f);
        r = fwrite(&t.cval, sizeof(char*), 1, f);
        if (r != 1) {
            fprintf(stderr, "Cislo se nepovedlo zapsat!!!\n");
            return;
        }
    }
    if (fclose(f) == EOF) {
        fprintf(stderr, "Soubor se nepovedlo zavrit!!!\n");
        return;
    }
}
void read_struct_from_bin() {
    FILE *f = fopen("input.bin", "rb");
    if (f == NULL) {
        fprintf(stderr, "Nepovedlo se!!!\n");
        return;
    }
    test_t t;
    for (int i = 0; i < 30; ++i) {
        size_t r = fread(&t, sizeof(char), 1, f);
        r = fread(&t.ival, sizeof(char) + sizeof(int), 1, f);
        if (r != 1) {
            fprintf(stderr, "Cislo se nepovedlo zapsat!!!\n");
            return;
        }
//        printf("%d %c %c\n", t.ival, t.cval, t.znak);
    }
    if (fclose(f) == EOF) {
        fprintf(stderr, "Soubor se nepovedlo zavrit!!!\n");
        return;
    }
}
int le_to_be(int x) {
    unsigned char *a = (unsigned char*)&x;
    unsigned char *b = (unsigned char*)malloc(sizeof(int));
    for (size_t i = 0; i < sizeof(int); ++i)
        b[sizeof(int) - 1 - i] = a[i];
    return *(int*)b;
}
