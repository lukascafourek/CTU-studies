#ifndef SORTING_MERGESORT_PARALLEL_H
#define SORTING_MERGESORT_PARALLEL_H

#include "mergesort.h"
#include "mergesort_sequential.h"

constexpr size_t CUTOFF = 10000;

template <typename elem_t>
void mergesort_parallel_worker(elem_t* data, unsigned long size, elem_t* tmp){
    if(size <= CUTOFF){
        mergesort_sequential_worker(data,size,tmp);
        return;
    }

    size_t h_size = size / 2;

    // Nejprve si pole rozdelime na dve casti [a .. b-1] a [b .. end-1]
    elem_t* a = data;             // Pointer na zacatek segmentu
    elem_t* b = data + h_size;    // Pointer doprostred segmentu (konec leve poloviny)
    elem_t* end = data + size;    // Konec segmentu (konec prave poloviny)

    // Stejnym zpusobem si rozdelime i pomocne pole
    elem_t* tmp_a = tmp;
    elem_t* tmp_b = tmp + h_size;

    // Zavolame rekurzivni volani na levou a pravou polovinu

    #pragma omp task
    mergesort_parallel_worker(a, static_cast<unsigned long>(h_size), tmp_a);

    #pragma omp task
    mergesort_parallel_worker(b, static_cast<unsigned long>(size - h_size), tmp_b);

    #pragma omp taskwait
    // A vysledky nakonec slijeme pomoci operace merge
    merge(a, b, end, tmp);
}

template <typename elem_t>
void mergesort_parallel(std::vector<elem_t> & data) {
    const size_t size = data.size();
    std::vector<elem_t> tmp(size);

#pragma omp parallel
#pragma omp single
    mergesort_parallel_worker(&data[0],size,&tmp[0]);
}


template <typename T>
void my_mergesort_worker(std::vector<T>& v, size_t begin, size_t end) {
    if(end-begin <= CUTOFF){
        std::sort(v.begin() + begin, v.begin() + end + 1);
        return;
    }

    const size_t mid = (begin + end)/2;

    #pragma omp task shared(v,begin,mid)
    my_mergesort_worker(v,begin,mid);

    #pragma omp task shared(v,mid,end)
    my_mergesort_worker(v,mid+1,end);

    #pragma omp taskwait

    std::inplace_merge(v.begin() + begin, v.begin() + mid + 1,v.begin() + end + 1);
}

template <typename T>
void my_mergesort_parallel(std::vector<T>& v){
#pragma omp parallel
#pragma omp single
    my_mergesort_worker(v,0,v.size()-1);
}




#endif //SORTING_MERGESORT_PARALLEL_H

