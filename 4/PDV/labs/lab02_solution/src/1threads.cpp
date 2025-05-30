#include "1threads.h"
#include "../pdv_lib/pdv_lib.hpp"
#include <cstddef>
#include <algorithm>
// #include <execution>
#include <thread>
#include <mutex>

const size_t THREAD_COUNT = std::thread::hardware_concurrency();

void map_sequential(std::vector<float>& data, MapFn map_fn) {
    for (float& f : data) {
        f = map_fn(f);
    }
    // C ekvivalent:
    //for (size_t i = 0; i < data.size(); i++) {
    //    data[i] = map_fn(data[i]);
    //}
}

void map_std_seq(std::vector<float>& data, MapFn map_fn) {
    std::transform(data.begin(), data.end(), data.begin(), map_fn);
}

void map_std_par([[maybe_unused]] std::vector<float>& data, [[maybe_unused]] MapFn map_fn) {
    // zakomentovano, protoze na Linuxu vyzaduje pouziti paralelni exekuce extra knihovnu
    //std::transform(std::execution::par_unseq, data.begin(), data.end(), data.begin(), map_fn);
}

void map_openmp(std::vector<float>& data, MapFn map_fn) {
    #pragma omp parallel for
    for (float& f : data) {
        f = map_fn(f);
    }
}


/** Naivni implementace paralelizace se sdilenym indexem. */
void map_manual(std::vector<float>& data, MapFn map_fn) {
    size_t i = 0;
    auto process = [&] {
        while (i < data.size()) {
            // kazde vlakno si ve smycce vezme a incrementuje dalsi index ke zpracovani...
            auto& entry = data[i++];
            // ...a prvek zpracuje
            entry = map_fn(entry);
        }
    };

    std::vector<std::thread> worker_threads{};
    for (size_t j = 0; j < THREAD_COUNT; j++) {
        worker_threads.push_back(std::thread{process});
    }

    for (auto& thread : worker_threads) {
        thread.join();
    }
}

void map_manual_locking(std::vector<float>& data, MapFn map_fn) {
    std::mutex m{};
    size_t i = 0;
    auto process = [&] {
        while (true) {
            // MK: tahle konstrukce je trochu zvlastni, ale umoznuje mit celou kritickou sekci pohromade;
            //     osobne ji trochu preferuju pred tim, aby byl mutex zamceny pres okraj iterace loopu,
            //     ale nevadi mi ani jedno
            size_t i_local;
            {
                std::unique_lock<std::mutex> lock(m);
                i_local = i++;
            }

            if (i_local >= data.size()) {
                break;
            }

            auto& item = data[i_local];
            item = map_fn(item);
        }
    };

    std::vector<std::thread> worker_threads{};
    for (size_t j = 0; j < THREAD_COUNT; j++) {
        worker_threads.push_back(std::thread{process});
    }

    for (auto& thread : worker_threads) {
        thread.join();
    }
}

void map_manual_atomic(std::vector<float>& data, MapFn map_fn) {
    std::atomic<size_t> i = 0;
    auto process = [&] {
        while (true) {
            // je potreba mit pouze 1 atomickou operaci, tady poslouzi inkrementace, ktera zaroven vraci puvodni hodnotu
            auto i_local = i++;
            if (i_local >= data.size()) {
                break;
            }
            auto& item = data[i_local];
            item = map_fn(item);
        }
    };

    std::vector<std::thread> worker_threads{};
    for (size_t j = 0; j < THREAD_COUNT; j++) {
        worker_threads.push_back(std::thread{process});
    }

    for (auto& thread : worker_threads) {
        thread.join();
    }
}

void map_manual_ranges(std::vector<float>& data, MapFn map_fn) {
    // thread zpracuje prvky od indexu `start_i` po index `end_i`
    auto process = [&](size_t start_i, size_t end_i) {
        for (size_t i = start_i; i < end_i; i++) {
            data[i] = map_fn(data[i]);
        }
    };

    auto batch_size = data.size() / THREAD_COUNT;
    // prvnich `leftover` vlaken dostane 1 prvek navic na zpracovani
    auto leftover = data.size() % THREAD_COUNT;

    size_t next_start_i = 0;
    std::vector<std::thread> worker_threads{};
    for (size_t j = 0; j < THREAD_COUNT; j++) {
        size_t end_i = next_start_i + batch_size + (j < leftover ? 1 : 0);
        // predame napocitane indexy na zpracovani vlaknu
        worker_threads.push_back(std::thread{process, next_start_i, end_i});
        next_start_i = end_i;
    }

    for (auto& thread : worker_threads) {
        thread.join();
    }
}
