#pragma once

#include <cstddef>
#include <vector>
#include <functional>
#include <atomic>
#include <omp.h>

template<typename row_t>
using predicate_t = std::function<bool(const row_t&)>;

template<typename row_t>
bool is_satisfied_for_all(std::vector<predicate_t<row_t>> predicates, std::vector<row_t> data_table);

template<typename row_t>
bool is_satisfied_for_any(std::vector<predicate_t<row_t>> predicates, std::vector<row_t> data_table);

template<typename row_t>
bool is_satisfied_for_all(std::vector<predicate_t<row_t>> predicates, std::vector<row_t> data_table) {
    std::atomic<bool> result{true};
    #pragma omp parallel
    #pragma omp for
    for (size_t i = 0; i < predicates.size(); i++) {
        #pragma omp cancellation point for
        auto& predicate = predicates[i];
        bool local_result = false;
        for (size_t j = 0; j < data_table.size(); j++) {
            #pragma omp cancellation point for
            if (predicate(data_table[j])) {
                local_result = true;
                break;
            }
        }
        if (!local_result && result) {
            result = false;
            #pragma omp cancel for
        }
    }
    return result;
}

template<typename row_t>
bool is_satisfied_for_any(std::vector<predicate_t<row_t>> predicates, std::vector<row_t> data_table) {
    std::atomic<bool> result(false);
    #pragma omp parallel
    #pragma omp for
    for (size_t j = 0; j < data_table.size(); j++) {
        #pragma omp cancellation point for
        for (size_t i = 0; i < predicates.size(); i++) {
            #pragma omp cancellation point for
            auto& predicate = predicates[i];
            if (predicate(data_table[j])) {
                result = true;
                #pragma omp cancel for
            }
        }
    }
    return result;
}
