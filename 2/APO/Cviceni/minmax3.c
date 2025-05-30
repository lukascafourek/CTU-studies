#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

#define SIZE 8388608
#define SIZE2 4096

long int pole[SIZE];

int main() {
  int i=0, ii=0;  
  for (; i<SIZE2;) {
    pole[i++] = (random()%10000) - 5000;
  }
  long int max=pole[0];
  long int min=pole[0];
  for (; ii<SIZE;) {
     for (int j=0; j<SIZE2; j++, ii++) {
        if (max<pole[ii]) {
           max = pole[ii];
        }
        if (min>pole[ii]) {
           min = pole[ii];
        }
     }
     if (ii<SIZE) {
        for (int j=0; j<SIZE2; j++) {
           pole[i++] = (random()%10000) - 5000;
        }
     }
  }
  printf("Min %li max %li\n", min, max);
  return 0;
}
