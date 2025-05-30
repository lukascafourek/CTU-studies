#include <omp.h>
#include "../pdv_lib/pdv_lib.hpp"

// V posledni sade uloh se zamerime na vypocet Fibonacciho cisla pomoci rekurze a budete si moci vyzkouset
// `#pragma omp task` pro rekurzivni paralelizaci. V testech pocitame cislo FIB_QUERY, ktere si muzete
// upravit ve funkci `main`.


// sekvencni implementace; je tu jednak pro porovnani rychlosti, a druhak proto, ze i v paralelni
//  implementaci na ni prepneme, kdyz uz mame dost vytvorenych tasku, abychom saturovali vsechna jadra CPU
uint64_t fibonacci_seq(uint64_t n) {
    if (n <= 1) return n;
    return fibonacci_seq(n - 1) + fibonacci_seq(n - 2);
}

uint64_t fibonacci_par(uint64_t n, uint64_t task_count) {
    if (task_count == 1) return fibonacci_seq(n);
//    if (n <= 1) return  n;
    uint64_t x, y;
    #pragma omp task shared(x)
    x = fibonacci_par(n - 1, task_count >> 1);
    y = fibonacci_par(n - 2, task_count - (task_count >> 1));
    #pragma omp taskwait
    return x + y;
}

uint64_t fib(uint64_t n) {
    uint64_t r;
    #pragma omp parallel
    #pragma omp single
    r = fibonacci_par(n, omp_get_num_threads() * 8);
    return r;
}

int main() {
    constexpr uint64_t FIB_QUERY = 42;

    pdv::benchmark("fibonacci_seq", 5, [] {
        return fibonacci_seq(pdv::launder_value(FIB_QUERY));
    });

    pdv::benchmark("fibonacci_par", 5, [] {
        return fib(pdv::launder_value(FIB_QUERY));
    });

    return 0;
}
