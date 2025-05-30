#include <stdio.h>
 
int main(int argc, char *argv[])
{
    int arr[5] = { 1, 2, 3, 4, 5 };
 
    for (int i = 0; i < 10000; ++i){
        printf("%d: %d\n", i, arr[i]);
    }
 
    return 0;
}
