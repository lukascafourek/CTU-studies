#ifndef SORTING_PREFIXSUM_H
#define SORTING_PREFIXSUM_H

#include <cstdlib>
#include <chrono>
#include <iostream>
#include <omp.h>
#include <vector>
#include <cmath>

template <typename T>
void prefix_sum_sequential(T * data, const size_t size);

template <typename T>
void prefix_sum_parallel(T * data, const size_t size);



template <typename T>
void prefix_sum_sequential(T * data, const size_t size) {
    for(size_t i = 1 ; i < size ; i++) {
        data[i] += data[i-1];
    }
}


template <typename T>
void prefix_sum_parallel(T * data, const size_t size) {
    //každé vlákno bude akumulovat součet své části pole do svého vlastniho bucketu
    std::vector<T> sums(omp_get_max_threads());

#pragma omp parallel default(none) shared(sums,size,data)
    {
        //rozdelime praci mezi vlakna
        size_t chunk_size = 1 + static_cast<unsigned>(size) / omp_get_num_threads();
        size_t thread_id = omp_get_thread_num();
        size_t begin = thread_id * chunk_size;
        size_t end = (thread_id + 1) * chunk_size;
        if(end > size) end = size; //musime osetrit pripad, kdy nam konec zasahne mimo range vektoru

        //kazde vlakno zde secte svoji vlastni cast pole
        T acc = 0;
        for(size_t i = begin;i<end;i++){
            acc += data[i];
        }
        //"parcialni" sumu nahrajeme do bucketu prislusici vlaknu, ktere sumu zpracovalo
        sums[thread_id] = acc;

        //zde musime pockat nez vsechna vlakna dokonci "parcialni" sumy
#pragma omp barrier

        //kazde vlakno akumuluje vysledky vlaken pod nim
        T pre = 0;
        for(unsigned int i = 0; i < thread_id; i++){
            pre += sums[i];
        }
        //nasledne se akumulovany vysledek pricte do "nulteho" prvku pro dane vlakno
        data[begin] += pre;
        //prvni prvek je zpracovany a jsou v nem vsechny data ze vsech vlaken s nizsim ID
        for(size_t i = begin+1;i<end;i++){
            data[i] += data[i-1];
        }
    }

}


#endif //SORTING_PREFIXSUM_H
