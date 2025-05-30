#include <stdio.h>
#include <stdlib.h>

#define EXIT_SUCCESS 0
#define ERROR_RANGE 100
#define MIN_VALUE -10000
#define MAX_VALUE 10000

int read_input(int num, int ret);
void print_error(void);
int print_num(int num, int count);
int positive_num(int num, int positive);
int negative_num(int num, int negative);
int even_num(int num, int even);
int odd_num(int num, int odd);
int sum_num(int num, int sum);
double average_num(int sum, int count, double average);
int max_num(int num, int max);
int min_num(int num, int min);
void print_stats(int count, int positive, int negative, int even, 
                int odd, int max, int min, double average);

int main(int argc, char *argv[]) {
  int count = 0, positive = 0, negative = 0, even = 0, odd = 0, sum = 0, 
      num, max = MIN_VALUE, min = MAX_VALUE, ret = EXIT_SUCCESS; 
  double average = 0;
  while (scanf("%d", &num) == 1) {
    ret = read_input(num, ret);
    if (ret == EXIT_SUCCESS) {
      count = print_num(num, count);
      positive = positive_num(num, positive);
      negative = negative_num(num, negative);
      even = even_num(num, even);
      odd = odd_num(num, odd);
      sum = sum_num(num, sum);
      max = max_num(num, max);
      min = min_num(num, min);
    }
    else {
      print_error();
      return ret;
    }
  }
  average = average_num(sum, count, average);
  print_stats(count, positive, negative, even, odd, max, min, average);
  return ret;
}
int read_input(int num, int ret) {    // this function reads input
  if (num < MIN_VALUE || num > MAX_VALUE) {
    ret = ERROR_RANGE;
  }
  return ret;
}
void print_error(void) {    // this prints error message
  fprintf(stdout, "\nError: Vstup je mimo interval!\n");
}
int print_num(int num, int count) {    // this function prints input 
  if (count <= 0) {                    // and returns input count
    printf("%d", num);
  }      
  else {
    printf(", %d", num);
  }
  count++;
  return count;
}
int positive_num(int num, int positive) {    // this returns positive count
  if (num > 0) {
    positive++;
  }
  return positive;
}
int negative_num(int num, int negative) {    // this returns negative count
  if (num < 0) {
    negative++;
  }
  return negative;
}
int even_num(int num, int even) {    // this function returns even count
  if (num % 2 == 0) {
    even++;
  }
  return even;
}
int odd_num(int num, int odd) {    // this function returns odd count
  if (num % 2 != 0) {
    odd++;
  }
  return odd;
}
int sum_num(int num, int sum) {    // this returns sum of inputs
  sum += num;
  return sum;
}
double average_num(int sum, int count, double average) { // this returns average
  average = (double)sum / count;                         // using sum and count
  return average;
}
int max_num(int num, int max) {    // this returns input with maximum value
  if (num > max) {
    max = num;
  }
  return max;
}
int min_num(int num, int min) {    // this returns input with minimum value
  if (num < min) {
    min = num;
  }
  return min;
}
void print_stats(int count, int positive, int negative, int even,
                int odd, int max, int min, double average) {   // this prints
  printf("\nPocet cisel: %d", count);                          // statistics
  printf("\nPocet kladnych: %d", positive);
  printf("\nPocet zapornych: %d", negative);
  printf("\nProcento kladnych: %.2lf", ((double)positive / count) * 100);
  printf("\nProcento zapornych: %.2lf", ((double)negative / count) * 100);
  printf("\nPocet sudych: %d", even);
  printf("\nPocet lichych: %d", odd);
  printf("\nProcento sudych: %.2lf", ((double)even / count) * 100);
  printf("\nProcento lichych: %.2lf", ((double)odd / count) * 100);
  printf("\nPrumer: %.2lf", average);
  printf("\nMaximum: %d", max);
  printf("\nMinimum: %d\n", min);
}
