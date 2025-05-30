#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef MAX_NUM
#define MAX_NUM 10
#endif
#define  INIT_SIZE 13
enum {
    ERROR_OK = EXIT_SUCCESS,
    ERROR_MEM = 101,
    ERROR_OUT = 102
};

void init(unsigned int a[], unsigned int n);
//void print(unsigned int a[], unsigned int n);
void permute(unsigned int a[], unsigned int n);
char* read(int *error);
char** read_lines(int *error, int *n);
void free_lines(size_t n, char ***str);
int print(char *str);

int main() {
    int error = ERROR_OK;
    int n = 0;
    char** lines = read_lines(&error, &n);
    if (!error) {
        unsigned int* a = (unsigned int*)malloc(sizeof(unsigned int) * n);
        if (a) {
            init(a, n);
    //        print(a, n);
            permute(a, n);
    //        print(a, n);
            for (int i = 0; i < n && error == ERROR_OK; ++i)
                error = print(lines[a[i] - 1]);
            free_lines(n, &lines);
        } else {
            error = ERROR_MEM;
            free_lines(n, &lines);
        }

    }
    return error;
}
void init(unsigned int a[], unsigned int n) {
    for (size_t i = 0; i < n; ++i)
        a[i] = i + 1;
}
/*void print(unsigned int a[], unsigned int n) {
    for (size_t i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");
}*/
void permute(unsigned int a[], unsigned int n) {
    srand(time(NULL));
    for (size_t i = 0; i < n; ++i) {
        unsigned int r = rand() % n;
        unsigned int t = a[i];
        a[i] = a[r];
        a[r] = t;
    }
}
char* read(int *error) {
    size_t size = INIT_SIZE;
    char* str = (char*)malloc(sizeof(char) * (size + 1));
    size_t i = 0;
    if (str) {
        char r;
        while ((r = getchar()) != EOF) {
            if (i == size) {
                char *t =  (char*)realloc(str, sizeof(char) * (2 * size + 1));
                if (!t) {
                    free(str);
                    str = NULL;
                    *error = ERROR_MEM;
                    break;
                }
                str = t;
                size *= 2;
            }
            str[i++] = r;
            if (r == '\n')
                break;
        }
    }
    else
        *error = ERROR_MEM;
    if (str && i > 0) {
        str[i++] = '\0';
        if (i < size) {
            char *t =  (char*)realloc(str, sizeof(char) * i);
            if (!t) {
                free(str);
                str = NULL;
                *error = ERROR_MEM;
            }
            str = t;            
        }
    }
    if (str && i == 0) {
        free(str);
        str = NULL;
    }
    return str;
}
char** read_lines(int *error, int *n) {
    size_t size = INIT_SIZE;
    char** lines = (char**)malloc(sizeof(char*) * (size));
    size_t i = 0;
    if (lines) {
        char* r;
        while ((r = read(error)) != NULL && !(*error)) {
            if (i == size) {
                char **t =  (char**)realloc(lines, sizeof(char*) * (2 * size));
                if (!t) {
                    free_lines(i, &lines);
                    *error = ERROR_MEM;
                    break;
                }
                lines = t;
                size *= 2;
            }
            lines[i++] = r;
            fprintf(stderr, "DEBUG: read line %zu from %zu\n", i, size);
        }
    }
    else {
        *error = ERROR_MEM;
        free_lines(i, &lines);
    }
    if (lines && i > 0) {
        if (i < size) {
            char **t =  (char**)realloc(lines, sizeof(char*) * i);
            if (!t) {
                free(lines);
                *error = ERROR_MEM;
            }
            lines = t;            
        }
    }
    *n = i;
    return lines;
}
void free_lines(size_t n, char ***str) {
    if (str && *str) {
        for (int i = 0; i < n; ++i) {
	        free((*str)[i]);
        }
        free(*str);
    }
    *str = NULL;
} 
int print(char *str) {
   int ret = ERROR_OK;
   size_t i = 0;
   while (str && str[i] != '\0') {
      if (putchar(str[i++]) == EOF) {
         ret = ERROR_OUT;
         break;
      }
   }
   return ret;
}
