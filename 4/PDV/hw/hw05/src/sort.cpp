#include "sort.h"
#include <stdexcept>

void msd_radix_par(std::vector<std::string*>& vector_to_sort, MappingFunction mapping_function,
                    size_t alphabet_size, size_t str_size, size_t start, size_t end, size_t pos) {
    if (start >= end || pos >= str_size)
        return;
    std::vector<std::vector<std::string*>> buckets(alphabet_size);
    for (size_t i = start; i <= end; ++i) {
        size_t index = mapping_function((*vector_to_sort[i]).at(pos));
        buckets[index].push_back(vector_to_sort[i]);
    }
    #pragma omp parallel for schedule(static)
    for (size_t i = 0; i < alphabet_size; ++i) {
        msd_radix_par(buckets[i], mapping_function, alphabet_size, str_size, 0, buckets[i].size() - 1, pos + 1);
    }
    size_t index = start;
    for (size_t i = 0; i < alphabet_size; ++i) {
        for (auto& j: buckets[i]) {
            vector_to_sort[index++] = j;
        }
    }
}

// implementace vaseho radiciho algoritmu. Detalnejsi popis zadani najdete v "sort.h"
void radix_par(std::vector<std::string*>& vector_to_sort, MappingFunction mapping_function,
               size_t alphabet_size, size_t str_size) {
    if (vector_to_sort.empty() || str_size == 0)
        return;
    msd_radix_par(vector_to_sort, mapping_function, alphabet_size, str_size, 0, vector_to_sort.size() - 1, 0);
}
