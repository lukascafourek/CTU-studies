#include <cmath>
#include "../pdv_lib/pdv_lib.hpp"
#include "1threads.h"

float test_fn(float input) {
    for (size_t i = 0; i < 10; i++) {
        // very useful operation
        input = std::log(std::exp(input));
    }
    // add 1 so that we can check that all items were processed
    return input + 1.0f;
}

void benchmark(std::vector<float> data, std::string_view fn_name, void (* fn)(std::vector<float>&, MapFn)) {
    // 1 warmup iteration, 5 measured iterations
    pdv::benchmark(fn_name, 1, 5, [&] {
        fn(data, test_fn);
        // return a pointer to `data`, so that we get a checksum printed for validation (and avoid a copy)
        return &data;
    });
    pdv::do_not_optimize_away(data);
}

#define BENCHMARK(fn) benchmark(data, #fn, fn)

int main() {
    std::vector<float> data = pdv::generate_random_vector<float>(1'000'001); // +1 to catch bad batching :)

    // set format the result for the checksum print, a bit hacky
    std::cout << std::fixed << std::setprecision(2);

    BENCHMARK(map_sequential);
    BENCHMARK(map_std_seq);
    BENCHMARK(map_std_par);
    BENCHMARK(map_openmp);
    std::cout << "\n";
    BENCHMARK(map_manual);
    BENCHMARK(map_manual_locking);
    BENCHMARK(map_manual_atomic);
    BENCHMARK(map_manual_ranges);
}
