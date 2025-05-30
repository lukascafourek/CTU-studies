#include <cmath>
#include "../pdv_lib/benchmark.hpp"
#include "2integrate.hpp"

// parametry pro integraci:
// pocatek intervalu, kde se zacina integrovat
constexpr double INTEGRATION_START = 0.0;
// velikost kroku behem integrace
constexpr double STEP_SIZE = 0.05;


// Jednoducha integracni funkce - identita
// Vypocet trva velmi kratce
double identity(double x) {
    return x;
}

// Slozitejsi integracni funkce - integral ze sinu s pohyblivou horni mezi integrace
// Vypocet trva tim dele, cim vetsi je argument udavajici delku integrace
double integrated_function(double integrationRange) {
    const size_t num_steps = 1000 * (size_t)(std::ceil(
            integrationRange * integrationRange * integrationRange * integrationRange));
    const double step_size = (integrationRange * integrationRange) / (double)num_steps;

    return integrate_sequential([](double x) { return std::sin(x); },
                                INTEGRATION_START, step_size, num_steps);
}


#define TEST(TESTED_FN) pdv::benchmark(#TESTED_FN, [&] { \
    return TESTED_FN(fn, INTEGRATION_START, STEP_SIZE, step_count); \
})

void benchmark_on_fn(Integrand fn, size_t step_count) {
    pdv::benchmark_group(30);

    TEST(integrate_sequential);
    TEST(integrate_omp_critical);
    TEST(integrate_omp_atomic);
    TEST(integrate_omp_reduction);

    TEST(integrate_omp_for_static);
    TEST(integrate_omp_for_dynamic);
}

int main() {
    std::cout << "Integrace slozitejsi funkce `integrated_function(...)` - "
                 "numericky aproximovany integral sin(x) s promenlivym poctem kroku\n";
    benchmark_on_fn(&integrated_function, 120);

    std::cout << "\n";

    std::cout << "Integrace `identity(...)`: identity(x)=x\n";
    // uvazujeme vice kroku, protoze funkce f(x)=x je opravdu jednoducha
    benchmark_on_fn(&identity, 5'000'000);

    return 0;
}
