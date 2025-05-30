#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define EXIT_SUCCESS 0
#define ERROR_PATTERN 1
#define ALLOC_CHAR 20
#define ERROR_OPEN 100
#define ERROR_CLOSE 101
#define LINE_PROCESS 10
#define END_OF_LINE 11
#define END_OF_FILE 12

typedef struct {
  char* line;
  int size;
  int capacity;
} string_line;

int open_file(FILE** f, char* filename, int ret);
int close_file(FILE** f, int ret);
void alloc_line(string_line* l);
void realloc_line(string_line* l);
void free_line(string_line* l);
int read_line(string_line* l, FILE* f);
bool find_line(string_line l, char* pattern);
int pattern_length(char* pattern);

int main(int argc, char *argv[]) {
  string_line line;
  FILE* f = NULL;
  int ret = EXIT_SUCCESS;
  if (argv[2]) {
    ret = open_file(&f, argv[2], ret);
  }
  if (ret == EXIT_SUCCESS) {
    int read = LINE_PROCESS;
    int counter = 0;
    while (read != END_OF_FILE) {
      alloc_line(&line);
      if (argv[2]) {
        read = read_line(&line, f);
      }
      else {
        read = read_line(&line, stdin);
      }
      if (find_line(line, argv[1])){
        printf("%s\n", line.line);
        counter++;
      }
      free_line(&line);
    }
    if (counter == 0) {
      ret = ERROR_PATTERN;
    }
    if (argv[2]) {
      ret = close_file(&f, ret);
    }
  }
  return ret;
}
int open_file(FILE** f, char* filename, int ret) {  // this opens file
  if ((*f = fopen(filename, "r")) == NULL) {
    fprintf(stderr, "Soubor se nepodarilo otevrit!\n");
    ret = ERROR_OPEN;
  }
  return ret;
}
int close_file(FILE** f, int ret) {   // this closes file
  if(fclose(*f) == EOF) {
    fprintf(stderr, "Soubor se nepodarilo zavrit!\n");
    ret = ERROR_CLOSE;
  }
  return ret;
}
void alloc_line(string_line* l) {   // this allocates memory
  l->line = (char*)malloc(sizeof(char));
  l->size = 0;
  l->capacity = 1;
  l->line[0] = '\0';
}
void realloc_line(string_line* l) {   // this reallocates memory
  l->capacity += ALLOC_CHAR;
  l->line = realloc(l->line, l->capacity * sizeof(char));
}
void free_line(string_line* l) {    // this frees up memory
  if (l->line != NULL) {
    free(l->line);
  }
}
int read_line(string_line* l, FILE* f) {    // this reads given line
  int one_line = LINE_PROCESS;
  while (one_line == LINE_PROCESS) {
    char ch = fgetc(f);
    if (l->size + 2 >= l->capacity) {
      realloc_line(l);
    }
    switch (ch) {
      case '\n':
        one_line = END_OF_LINE;
        break;
      case EOF:
        one_line = END_OF_FILE;
        break;
      default:
        l->line[l->size++] = ch;
        break;
    }
  }
  l->line[l->size++] = '\0';
  return one_line;
}
bool find_line(string_line l, char* pattern) {  // this finds pattern in line
  int len = pattern_length(pattern);
  if (!len) {
    return true;
  }
  int i = 0;
  while (l.line[i]) {
    if (l.line[i] == pattern[0]) {
      int j = 1;
      while (j < len && i + j < l.size && 
      l.line[i + j] == pattern[j]) {
        j++;
      }
      if (j == len) {
        return true;
      }
    }
    i++;
  }
  return false;
}
int pattern_length(char* pattern) {   // this calculates pattern length
  int tmp = 0;
  while (pattern[tmp]) {
    tmp++;
  }
  return tmp;
}
