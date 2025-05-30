#include "integrate.h"

#include <omp.h>
#include <cmath>

double
integrate_sequential(std::function<double(double)> integrand, double a, double step_size, int step_count) {

    // Promenna kumulujici obsahy jednotlivych obdelniku
    double acc = 0.0;

    for(int i = 0 ; i < step_count ; i++) {
        double cx = a + (2*i+1) * step_size / 2.0;
        acc += integrand(cx) * step_size;
    }
    
    // Celkovy obsah aproximuje hodnotu integralu funkce
    return acc;
}

double
integrate_omp_critical(std::function<double(double)> integrand, double a, double step_size, int step_count) {
    double acc = 0.0;

    // Rozdelte celkovy interval na podintervaly prislusici jednotlivym vlaknum
    // Identifikujte kritickou sekci, kde musi dojit k zajisteni konzistence mezi vlakny
    #pragma omp parallel
    {
        const int threads = omp_get_num_threads();
        const int thread_id = omp_get_thread_num();
        const int chunk_size = 1+ step_count / threads;

        const int begin = chunk_size * thread_id;
        const int end = std::min(chunk_size * (thread_id + 1), step_count);

        for(int i = begin ; i < end ; i++) {
            double cx = a + (2*i+1) * step_size / 2.0;
			double rectangle = integrand(cx) * step_size;

            #pragma omp critical
            {
                acc += rectangle;
            }
        }
    }

    return acc;
}

double
integrate_omp_atomic(std::function<double(double)> integrand, double a, double step_size, int step_count) {
    double acc = 0.0;

    // Rozdelte celkovy interval na podintervaly prislusici jednotlivym vlaknum
    // Identifikujte kritickou sekci, kde musi dojit k zajisteni konzistence mezi vlakny
    #pragma omp parallel
    {
        const int threads = omp_get_num_threads();
        const int thread_id = omp_get_thread_num();
        const int chunk_size = 1 + step_count / threads;

        const int begin = chunk_size * thread_id;
        const int end = std::min(chunk_size * (thread_id + 1), step_count);

        for(int i = begin ; i < end ; i++) {
            double cx = a + (2*i+1) * step_size / 2.0;
			double rectangle = integrand(cx) * step_size;

            #pragma omp atomic
            acc += rectangle;
        }
    }

    return acc;
}

double integrate_omp_reduction(std::function<double(double)> integrand, double a, double step_size, int step_count) {
    double acc = 0.0;

    // Rozdelte celkovy interval na podintervaly prislusici jednotlivym vlaknum
    // Identifikujte kritickou sekci, kde musi dojit k zajisteni konzistence mezi vlakny
    #pragma omp parallel reduction(+:acc)
    {
        const int threads = omp_get_num_threads();
        const int thread_id = omp_get_thread_num();
        const int chunk_size = 1 + step_count / threads;

        const int begin = chunk_size * thread_id;
        const int end = std::min(chunk_size * (thread_id + 1), step_count);

        for(int i = begin ; i < end ; i++) {
            double cx = a + (2*i+1) * step_size / 2.0;
            acc += integrand(cx) * step_size;
        }
    }

    return acc;
}

double integrate_omp_for_static(std::function<double(double)> integrand, double a, double step_size, int step_count) {
    // Promenna kumulujici obsahy jednotlivych obdelniku
    double acc = 0.0;

    #pragma omp parallel for schedule(static) reduction(+:acc)
    for(int i = 0 ; i < step_count ; i++) {
        double cx = a + (2*i+1) * step_size / 2.0;
        acc += integrand(cx) * step_size;
    }
    
    // Celkovy obsah aproximuje hodnotu integralu funkce
    return acc;
}

double integrate_omp_for_dynamic(std::function<double(double)> integrand, double a, double step_size, int step_count) {
    // Promenna kumulujici obsahy jednotlivych obdelniku
    double acc = 0.0;

    #pragma omp parallel for schedule(dynamic) reduction(+:acc)
    for(int i = 0 ; i < step_count ; i++) {
        double cx = a + (2*i+1) * step_size / 2.0;
        acc += integrand(cx) * step_size;
    }
    
    // Celkovy obsah aproximuje hodnotu integralu funkce
    return acc;
}
