#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define INIT_SIZE 10

double* read_array(size_t *n);
void print_array(double* a, unsigned int n);
double average(double* a, unsigned int n);
double deviation(double* a, unsigned int n);

int main() {
    double input[] = {34.85, 28.11, 28.43, 32.09, 31.82, 29.22, 30.82, 32.46, 33.01, 29.02};
    int input2[] = {4, 2, 3, 5, 2, 2, 3, 4, 5, 6};
    size_t n = 0;
    double* array = read_array(&n);
    if (array) {
        for (size_t i = 0; i < INIT_SIZE; ++i) 
            array[i] = input[i];
        print_array(array, INIT_SIZE);
        printf("Prumer prvku: %.2lf", average(array, INIT_SIZE));
        printf("Smerodatna odchylka prvku: %.2lf", deviation(array, INIT_SIZE));
        free(array);
    }
    return 0;
}
void print_array(double* a, unsigned int n) {
    for (size_t i = 0; i < n; ++i) 
        printf("%.2lf ", a[i]);
    printf("\n");
}
double average(double* a, unsigned int n) {
    double suma = 0.0;
    for (size_t i = 0; i < n; ++i)
        suma += a[i];
    return suma / n;
}
double deviation(double* a, unsigned int n) {
    double suma = 0.0, avg = average(a, n);
    for (size_t i = 0; i < n; ++i)
        suma += (a[i] - avg) * (a[i] - avg);
    return sqrt(suma / (n - 1));
}
double* read_array(size_t *n) {
    size_t size = 0, capacity = INIT_SIZE;
    double* array = (double*)malloc(sizeof(double) * capacity);
    if (array) {
        double tmp;
        while (scanf("%lf", &tmp) == 1) {
            if (size == capacity) {
                double *t = (double*)realloc(array, sizeof(double) * capacity * 2);
                if (!t) {
                    free(array);
                    *n = 0;
                    return NULL;
                }
                capacity *= 2;
                array = t;
            }
            array[size++] = tmp;
        }
    }
    *n = size;
    return array;
}
