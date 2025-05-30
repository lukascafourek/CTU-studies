#include "../pdv_lib/pdv_lib.hpp"
#include <iostream>
#include <vector>

constexpr size_t MATRIX_SIZE = 10000;

// Nasledujici program pocita nasobeni matice vektorem, y = Ax
int main() {
    std::vector<double> A(MATRIX_SIZE * MATRIX_SIZE);
    std::vector<double> x(MATRIX_SIZE);
    std::vector<double> y(MATRIX_SIZE);

    // Vygenerujeme nahodnou matici A a vektor x
    pdv::uniform_random<double> random{0, 100};
    for (size_t i = 0; i < MATRIX_SIZE; i++) {
        x[i] = random();
        for (size_t j = 0; j < MATRIX_SIZE; j++) {
            A[i * MATRIX_SIZE + j] = random();
        }
    }

    // TODO: prevent autovectorization, otherwise compilers happily vectorize this and the comparison is more skewed than it should be
    pdv::benchmark("first i, second j", [&] {
        for (size_t i = 0; i < MATRIX_SIZE; i++) {
            for (size_t j = 0; j < MATRIX_SIZE; j++) {
                // Vsimnete si, ze vnitrni for smycka zpracovava po sobe jdouci prvky. Tyto prvky
                // uz mohou byt nactene v cache pameti procesoru, a my se tak vyhneme zbytecnym
                // pristupum do pameti.
                y[i] += A[i * MATRIX_SIZE + j] * x[j];
            }
        }
    });

    // Pro jistotu premazeme obsah y
    for (size_t i = 0; i < MATRIX_SIZE; i++) {
        y[i] = 0;
    }

    pdv::benchmark("first j, second i", [&] {
        for (size_t j = 0; j < MATRIX_SIZE; j++) {
            for (size_t i = 0; i < MATRIX_SIZE; i++) {
                // Naopak v teto implementaci, kdykoliv zmenime hodnotu promenne i ve vnitrni `for`
                // smycce, skocime v pameti o `MATRIX_SIZE * sizeof(double) bytu. Nevyuzijeme tak
                // princip lokality a donutime procesor nacitat data z pameti nacitat zbytecne
                // (protoze ve chvili, kdy se vratime k predchazejici hodnote i, procesor uz dany
                // blok pameti pravdepodobne "zapomnel" a bude ho muset nacist znova).
                y[i] += A[i * MATRIX_SIZE + j] * x[j];
            }
        }
    });
}
