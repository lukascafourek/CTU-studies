/** Implementation of core benchmarking functions. */
#pragma once

#include <atomic>
#include <chrono>
#include <utility>
#include <cassert>
#include "result.hpp"

namespace pdv {
    // source: https://github.com/martinus/nanobench/blob/b8d337a3bb1a430b5da6661bae430491fceb49c6/src/include/nanobench.h#L1027
    // do_not_optimize_away is a function which convinces the compiler that it's using the argument,
    // but actually is a no-op; useful to prevent the compiler from optimizing away our benchmarks
    // clang-format off
    #if defined(_MSC_VER)
        namespace _ {
            #pragma optimize("", off)
            inline void do_not_optimize_away_sink(const void*) {}
            #pragma optimize("", on)
        }

        template<typename Arg>
        inline void do_not_optimize_away(const Arg& arg) {
            _::do_not_optimize_away_sink(&arg);
        }
    #else
        template<typename T>
        inline void do_not_optimize_away(const T& val) {
            asm volatile("" : : "r,m"(val) : "memory");
        }

        template<typename T>
        inline void do_not_optimize_away(T& val) {
            #if defined(__clang__)
            asm volatile("" : "+r,m"(val) : : "memory");
            #else
            asm volatile("" : "+m,r"(val) : : "memory");
            #endif
        }
    #endif
    // clang-format on

    namespace _ {
        template<typename Callable>
        constexpr bool DefaultReturnsVoid = std::is_same_v<void, std::invoke_result_t<Callable>>;

        [[nodiscard]] inline auto get_benchmark_time() {
            // surround with barriers to prevent the compiler and CPU from being too clever with reordering
            std::atomic_thread_fence(std::memory_order_seq_cst);
            auto time = std::chrono::steady_clock::now();
            std::atomic_thread_fence(std::memory_order_seq_cst);
            return time;
        }

        template<typename BenchmarkFn>
        [[nodiscard]] inline benchmark_duration run_iteration(BenchmarkFn fn) {
            auto begin = get_benchmark_time();
            if constexpr (_::DefaultReturnsVoid<BenchmarkFn>) {
                fn();
            } else {
                // fn returns something, use it to ensure the value is not optimized away
                do_not_optimize_away(fn());
            }
            auto end = get_benchmark_time();
            return end - begin;
        }

        template<typename BenchmarkFn>
        [[nodiscard]] inline auto run_iteration_with_return_value(BenchmarkFn fn) {
            auto begin = get_benchmark_time();
            // FIXME: this will typically create a copy
            auto return_value = fn();
            auto end = get_benchmark_time();
            return std::make_pair(std::move(return_value), benchmark_duration(end - begin));
        }
    }

    struct benchmark_options {
        size_t iteration_count;
        size_t warmup_iteration_count;

        constexpr explicit benchmark_options(size_t iteration_count = 1, size_t warmup_iteration_count = 0)
                : iteration_count(iteration_count), warmup_iteration_count(warmup_iteration_count) {}
    };

    // overload used when `fn()` does not return anything
    template<typename BenchmarkFn, typename = std::enable_if_t<_::DefaultReturnsVoid<BenchmarkFn>>>
    [[nodiscard]] inline benchmark_result benchmark_raw(size_t iteration_count, BenchmarkFn fn) {
        assert(iteration_count > 0);

        std::vector<benchmark_duration> iter_durations{};
        iter_durations.reserve(iteration_count);

        for (size_t i = 0; i < iteration_count; i++) {
            iter_durations.push_back(_::run_iteration(fn));
        }

        return benchmark_result{iter_durations};
    }

    // overload used when `fn()` has a return value
    template<typename BenchmarkFn, typename = std::enable_if_t<!_::DefaultReturnsVoid<BenchmarkFn>>>
    [[nodiscard]] inline benchmark_result_with_value<std::invoke_result_t<BenchmarkFn>>
    benchmark_raw(size_t iteration_count, BenchmarkFn fn) {
        assert(iteration_count > 0);

        std::vector<benchmark_duration> iter_durations{};
        iter_durations.reserve(iteration_count);

        for (size_t i = 0; i < iteration_count - 1; i++) {
            iter_durations.push_back(_::run_iteration(fn));
        }

        // record the result of the last iteration
        // FIXME: test if this causes unnecessary copies of `return_value`
        auto [return_value, d] = _::run_iteration_with_return_value(fn);
        iter_durations.push_back(d);

        return benchmark_result_with_value{iter_durations, std::move(return_value)};
    }

    /**
     * Invokes `magic_operation()` `iteration_count` times and returns the average duration of a single
     * iteration. If `warmup_iteration_count` is non-zero, `magic_operation()` is executed before the measurement
     * is started to warmup caches, page in memory pages,...
     */
    template<typename BenchmarkFn>
    [[nodiscard]] inline auto benchmark_raw(const benchmark_options& options, BenchmarkFn fn) {
        assert(options.iteration_count > 0);
        if (options.warmup_iteration_count > 0) {
            // run the warmup iterations, ignore the measurement
            (void)benchmark_raw(options.warmup_iteration_count, fn);
        }
        // run the actual benchmark
        auto result = benchmark_raw(options.iteration_count, fn);
        result._set_warmup_iterations(options.warmup_iteration_count);
        return result;
    }
}
