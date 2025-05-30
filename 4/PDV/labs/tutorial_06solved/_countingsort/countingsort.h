#ifndef SORTING_COUNTINGSORT_H
#define SORTING_COUNTINGSORT_H

#include <vector>
#include <limits>
#include <cstdlib>
#include <chrono>
#include <omp.h>
#include "../_prefixsum/prefixsum.h"

template <typename elem_t>
void counting_sequential(std::vector<elem_t> & data) {
    elem_t min = std::numeric_limits<elem_t>::max();
    elem_t max = std::numeric_limits<elem_t>::min();

    for(auto && el : data) {
        if(el < min) min = el;
        if(el > max) max = el;
    }

    const size_t range = max - min + 1;
    std::vector<unsigned int> counts(range);

    for(auto && el : data) {
        ++counts[el-min];
    }

    unsigned int idx = 0;
    for(unsigned int i = 0 ; i < range ; i++) {
        unsigned int count = counts[i];
        unsigned int number = min + i;
        for(unsigned int k = 0 ; k < count ; k++) {
            data[idx++] = number;
        }
    }
}

template <typename elem_t>
void counting_parallel_atomic(std::vector<elem_t> & data) {

    elem_t minimum = std::numeric_limits<elem_t>::max();
    elem_t maximum = std::numeric_limits<elem_t>::min();

    // Provedeme nad polem redukci pocitajici minimum a maximum
    #pragma omp parallel for reduction(max:maximum) reduction(min:minimum)
    for(const auto& el : data) {
        if(el < minimum) minimum = el;
        if(el > maximum) maximum = el;
    }

    const size_t range = maximum - minimum + 1;

    std::vector<unsigned int> histograms(range);

    // Jednotlive cetnosti vyscitame atomicky
    // Prvni zpusob je vhodny, pokud data maji vetsi rozptyl
    // Vytvorime vektor atomickych promennych
    // Jednotlive cetnosti vyscitame atomicky
    #pragma omp parallel for
    for(const auto& el : data) {
        #pragma omp atomic
        histograms[el - minimum]++;
    }

    // Abychom mohli setridene pole vypisovat paralelne, musi kazde vlakno vedet,
    // na jake indexy ma zapisovat jake prvky, bez toho aby muselo cekat na
    // vypsani prvku mensich. Takovou informaci lze ziskat vypoctem prefixni
    // sumy (viz slidy).
    prefix_sum_parallel(&histograms[0], range);


    // Nakonec jiz staci pole vypsat. Diky prefixni sume presne vime,
    // jaky prvek ma byt na jakem indexu nezavisle na poctu mensich prvku
    #pragma omp parallel for
    for(unsigned int i = 0 ; i < range ; i++) {
        unsigned int begin  = i == 0 ? 0 : histograms[i - 1];
        unsigned int end = histograms[i];
        unsigned int number = minimum + i;
        for(unsigned int k = begin ; k < end ; k++) {
            data[k] = number;
        }
    }

}

template <typename elem_t>
void counting_parallel_histogram(std::vector<elem_t> & data) {

    elem_t minimum = std::numeric_limits<elem_t>::max();
    elem_t maximum = std::numeric_limits<elem_t>::min();

    unsigned int numThreads = omp_get_max_threads();

    // Provedeme nad polem redukci pocitajici minimum a maximum
    #pragma omp parallel for reduction(max:maximum) reduction(min:minimum)
    for(const auto& el : data) {
        if(el < minimum) minimum = el;
        if(el > maximum) maximum = el;
    }

    const size_t range = maximum - minimum + 1;

    // Druhy zpusob je lepsi, pokud ma kazdy prvek v poli mnoho vyskytu
    // Vytvorime si pro kazde vlakno jeden histogram
    std::vector<std::vector<unsigned int>> histogramPerThread(numThreads, std::vector<unsigned>(range));

    #pragma omp parallel
    {
        std::vector<unsigned int> &localHistogram = histogramPerThread[omp_get_thread_num()];
        // Histogram spocitame pro kazde vlakno zvlast
        #pragma omp for
        for (const auto &el: data) {
            localHistogram[el - minimum]++;
        }
    }

    // Nakonec vsechny histogramy slijeme dohromady
    #pragma omp parallel for
    for(unsigned int i = 0; i < range; i++){
        for(unsigned int j = 1; j < numThreads; j++){
            histogramPerThread[0][i] += histogramPerThread[j][i];
        }
    }
    auto& histogram = histogramPerThread[0];
    // Abychom mohli setridene pole vypisovat paralelne, musi kazde vlakno vedet,
    // na jake indexy ma zapisovat jake prvky, bez toho aby muselo cekat na
    // vypsani prvku mensich. Takovou informaci lze ziskat vypoctem prefixni
    // sumy (viz slidy).
    prefix_sum_parallel(&histogram[0], range);


    // Nakonec jiz staci pole vypsat. Diky prefixni sume presne vime,
    // jaky prvek ma byt na jakem indexu nezavisle na poctu mensich prvku
    #pragma omp parallel for
    for(unsigned int i = 0; i < range; i++) {
        unsigned int begin  = i == 0 ? 0 : histogram[i-1];
        unsigned int end = histogram[i];
        unsigned int number = minimum + i;
        for(unsigned int k = begin ; k < end ; k++) {
            data[k] = number;
        }
    }
}

#endif //SORTING_COUNTINGSORT_H

