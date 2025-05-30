#include "vector_sum.h"
#include <numeric>
#include <random>
#include <algorithm>

// typove aliasy pouzite v argumentech jsou definovane ve `vector_sum.h`
void vector_sum_omp_per_vector(const InputVectors& data, OutputVector& solution, size_t min_vector_size) {
    #pragma omp parallel for
    for (size_t i = 0; i < data.size(); i++) {
        int64_t sum = 0;
        for (auto n : data[i]) {
            sum += n;
        }
        solution[i] = sum;
    }
}

void vector_sum_omp_static(const InputVectors& data, OutputVector& solution, size_t min_vector_size) {
    #pragma omp parallel for schedule(static)
    for (size_t i = 0; i < data.size(); i++) {
        int64_t sum = 0;
        for (auto n : data[i]) {
            sum += n;
        }
        solution[i] = sum;
    }
}

void vector_sum_omp_dynamic(const InputVectors& data, OutputVector& solution, size_t min_vector_size) {
    #pragma omp parallel for schedule(dynamic)
    for (size_t i = 0; i < data.size(); i++) {
        int64_t sum = 0;
        for (auto n : data[i]) {
            sum += n;
        }
        solution[i] = sum;
    }
}

void vector_sum_omp_shuffle(const InputVectors& data, OutputVector& solution, size_t min_vector_size) {
    std::vector<size_t> pointers(data.size());
    std::iota(pointers.begin(), pointers.end(), 0);
    std::shuffle(pointers.begin(), pointers.end(), std::mt19937(std::random_device()()));
    #pragma omp parallel for schedule(static)
    for (size_t i = 0; i < data.size(); i++) {
        size_t idx = pointers[i];
        int64_t sum = 0;
        for (auto n : data[idx]) {
            sum += n;
        }
        solution[idx] = sum;
    }
}
