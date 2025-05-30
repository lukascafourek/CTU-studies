#include <iostream>
#include <vector>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <immintrin.h>

int popcnt_intrinsic(uint64_t x) {
	return static_cast<int>(_mm_popcnt_u64(x));
}

int popcnt_naive(uint64_t x) {
	int count = 0;
	for(uint64_t i = 1ULL << 63 ; i > 0 ; i >>= 1) {
		if(x & i) count++;
	}
	return count;
}


int main() {
	using namespace std::chrono;

	constexpr size_t N = 10000000;
	std::vector<unsigned int> x(N);
	std::vector<unsigned int> y(N);

	for(unsigned int i = 0 ; i < N ; i++) {
		x[i] = rand() + ((unsigned long long)rand() << 32);
		y[i] = rand() + ((unsigned long long)rand() << 32);
	}

	unsigned int count_popcnt = 0;
	unsigned int count_naive = 0;

	unsigned long elapsedIntristic = 0ULL;
	unsigned long elapsedNaive = 0ULL;

	{
		auto begin = steady_clock::now();
		for(unsigned int i = 0 ; i < N ; i++) {
			count_naive += popcnt_naive(x[i] & y[i]);
		}
		auto end = steady_clock::now();
		elapsedNaive = static_cast<unsigned long>(duration_cast<microseconds>(end-begin).count());
		printf("Cas naivni verze:               %dms  \tspeedup 1x\n",elapsedNaive);
	}

	{
		try {
			auto begin = steady_clock::now();
			for (unsigned int i = 0; i < N; i++) {
				count_popcnt += popcnt_intrinsic(x[i] & y[i]);
			}
			auto end = steady_clock::now();
			elapsedIntristic = static_cast<unsigned long>(duration_cast<microseconds>(end - begin).count());
			double speedup = (double) elapsedNaive / elapsedIntristic;
			printf("Cas intristicke verze:          %dms  \tspeedup %.2fx\n", elapsedIntristic, speedup);
		}
		catch(...){
			printf("Cas intristicke verze:          --- not implemented ---\n");
		}
	}
    
	printf("Vysledek je %s\n", (count_naive == count_popcnt) ? "spravny" : "spatny");

	return 0;
}