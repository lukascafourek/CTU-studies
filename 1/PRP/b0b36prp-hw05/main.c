#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define INIT_SIZE 10
#define EXIT_SUCCESS 0
#define ERROR_INPUT 100
#define ERROR_LENGTH 101
#define ERROR_MEM 102

char* read(int* ret);
void free_mem(char **str);
int check_errors(int *ret, char* str1, char* str2);
void print_error(int *ret);
void shift(int* ret, char* str1, char* str2);
int compare(int count, char* str1, char* str2);
char* rotate(char *str1);

int main(int argc, char *argv[]) {
    int ret = EXIT_SUCCESS;
    char* str1 = read(&ret);
    char* str2 = read(&ret);
    ret = check_errors(&ret, str1, str2);
    if (ret == EXIT_SUCCESS) {
        shift(&ret, str1, str2);
        if (ret != EXIT_SUCCESS) {
            print_error(&ret);
        }
    }
    else {
        print_error(&ret);
    }
    free_mem(&str1);
    free_mem(&str2);
    return ret;
}
char* read(int* ret) {  // this reads input and allocates memory for it
    size_t size = INIT_SIZE;
    char* str = (char*)malloc(sizeof(char) * (size));
    size_t i = 0;
    if (str) {
        char input;
        while (scanf("%c", &input) != EOF) {
            if (i == size) {
                char *t =  (char*)realloc(str, sizeof(char) * (2 * size));
                if (!t) {
                    free(str);
                    str = NULL;
                    *ret = ERROR_MEM;
                    break;
                }
                str = t;
                size *= 2;
            }
            if (input == '\n') {
                break;
            }
            str[i++] = input;
        }
    }
    else {
        *ret = ERROR_MEM;
    }
    if (str && i > 0) {
        str[i++] = '\0';
        if (i < size) {
            char *t =  (char*)realloc(str, sizeof(char) * i);
            if (!t) {
                free(str);
                str = NULL;
                *ret = ERROR_MEM;
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
void free_mem(char **str) {    // this frees up memory
    if (*str) {
        free(*str);
    }
    *str = NULL;
}
int check_errors(int *ret, char* str1, char* str2) { // this checks for errors
    int i = 0;
    if (*ret == EXIT_SUCCESS) {
        if (strlen(str1) != strlen(str2)) {
            *ret = ERROR_LENGTH;
        }
        while (str1[i] != '\0' && str2[i] != '\0') {
            if (!isalpha(str1[i]) || !isalpha(str2[i])) {
                *ret = ERROR_INPUT;
            }
            i++;
        }
    }
    return *ret;
}
void print_error(int *ret) { // this prints error
    switch (*ret) {
        case ERROR_MEM:
            fprintf(stderr, "Error: Chyba pameti!\n");
            break;
        case ERROR_INPUT:
            fprintf(stderr, "Error: Chybny vstup!\n");
            break;
        case ERROR_LENGTH:
            fprintf(stderr, "Error: Chybna delka vstupu!\n");
            break;
    }
}
// this below shifts input via function rotate and prints decoded message
void shift(int* ret, char* str1, char* str2) {    
    char* message = (char*)malloc(sizeof(char) * strlen(str1) + 1);
    if (message) {
        strcpy(message, str1);
        int max = 0;
        for (int j = 0; j < 52; ++j) {
            int count = 0;
            str1 = rotate(str1);
            count = compare(count, str1, str2);
            if (max < count) {
                max = count;
                for (int i = 0; str1[i] != '\0'; ++i) {
                    message[i] = str1[i];
                }
            }
        }
        for (int j = 0; message[j] != '\0'; ++j) {
            printf("%c", message[j]);
        }
        printf("\n");
        free(message);
        message = NULL;
    }
    else {
        *ret = ERROR_MEM;
    }
}
char* rotate(char *str1) {  // this shifts given string
    for (int i = 0; str1[i] != '\0'; ++i) {
        if (str1[i] >= 'a' && str1[i] <= 'z') {
            str1[i] += 1;
            if (str1[i] > 'z') {
                str1[i] = str1[i] - 'z' + 'A' - 1;
            }
        }
        else if (str1[i] >= 'A' && str1[i] <= 'Z') {
            str1[i] += 1;
            if (str1[i] > 'Z') {
                str1[i] = str1[i] - 'Z' + 'a' - 1;
            }
        }
    }
    return str1;
}
int compare(int count, char* str1, char* str2) {    // this compares given
    int i = 0;                                      // string with intercepted
    while (str1[i] != '\0' && str2[i] != '\0') {    // text
        if (str1[i] == str2[i]) {
            count += 1;
        }
        i++;
    }
    return count;
}
