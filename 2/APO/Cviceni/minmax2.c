#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

#define SIZE 8388608

long int pole[SIZE];

int main() {
  for (int i=0; i<SIZE; i++) {
    pole[i] = (random()%10000) - 5000;
  }
  long int max=pole[0];
  long int min=pole[0];
  for (int i=1; i<SIZE; i++) {
      if (max<pole[i]) {
          max = pole[i];
      }
      if (min>pole[i]) {
          min = pole[i];
      }
  }
  printf("Min %li max %li\n", min, max);
  return 0;
}
