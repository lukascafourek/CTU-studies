#include <iostream>
#include <vector>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <immintrin.h>

int log2_lzcnt(uint64_t x) {
	return static_cast<int>(63 - _lzcnt_u64(x));
}

int log2_naive(uint64_t x) {
	return (int)log2(x);
}


int main() {
	using namespace std::chrono;

	constexpr size_t N = 10000000;
	std::vector<uint64_t> data(N);

	for(unsigned int i = 0 ; i < N ; i++) {
		data[i] = rand() + ((uint64_t)rand() << 32);
	}

	std::vector<int> output_lzcnt(N);
	std::vector<int> output_naive(N);

	unsigned long elapsedNaive = 0ULL;
	unsigned long elapsedLZNT = 0ULL;

	{
		auto begin = steady_clock::now();
		for(unsigned int i = 0 ; i < N ; i++) {
			output_naive[i] = log2_naive(data[i]);
		}
		auto end = steady_clock::now();
		elapsedNaive = static_cast<unsigned long>(duration_cast<microseconds>(end-begin).count());
		printf("Cas naivni verze:               %dms  \tspeedup 1x\n",elapsedNaive);
	}

	{
		try {
			auto begin = steady_clock::now();
			for (unsigned int i = 0; i < N; i++) {
				output_lzcnt[i] = log2_lzcnt(data[i]);
			}
			auto end = steady_clock::now();
			elapsedLZNT = static_cast<unsigned long>(duration_cast<microseconds>(end - begin).count());
			double speedup = (double) elapsedNaive / elapsedLZNT;
			printf("Cas LZCNT verze:                %dms  \tspeedup %.2fx\n", elapsedLZNT, speedup);
		}
		catch(...){
			printf("Cas LZCNT verze:                --- not implemented ---\n");
		}
	}

	unsigned int wrong = 0;
	for(unsigned int i = 0 ; i < N ; i++) {
		if(output_lzcnt[i] != output_naive[i]) wrong++;
	}
	printf("Pocet prvku spoctenych spatne:  %d\n",wrong);

	return 0;
}