#include <stdio.h>
#include <stdlib.h>
 
#define EXIT_SUCCESS 0
#define ERROR_INPUT 100
 
typedef struct matrix {
  int rows;
  int cols;
  int* vals;
} matrix;
 
matrix *allocate_matrix(int rows, int cols);
void deallocate_matrix(matrix **m);
matrix *read_matrix(void);
void print_matrix(const matrix *const m);
char read_operator(int *ret);
matrix *sum(const matrix *const m1, const matrix *const m2, int sign);
matrix *multiplicate(const matrix *const m1, const matrix *const m2);
 
int main(int argc, char *argv[]) {
  int ret = EXIT_SUCCESS;
  matrix *m1 = read_matrix();
  char op = read_operator(&ret);
  matrix *m2 = read_matrix();
  int sign = 1;
  matrix *result = NULL;
  if (ret == EXIT_SUCCESS) {
    switch(op) {
      case '-':
        sign = -1;
      case '+':
        result = sum(m1, m2, sign);
        break;
      case '*':
        result = multiplicate(m1, m2);
        break;
    }
  }
  if (!result) {
    fprintf(stderr, "Error: Chybny vstup!\n");
    ret = ERROR_INPUT;
  }
  else {
    print_matrix(result);
  }
  deallocate_matrix(&m1);
  deallocate_matrix(&m2);
  deallocate_matrix(&result);
  return ret;
}
void print_matrix(const matrix *const m) {   // this prints the result
  if (m && m->vals) {
    printf("%d %d\n", m->rows, m->cols);        
    for (int r = 0; r < m->rows; ++r) {
      printf("%d", m->vals[r * m->cols]);
      for (int c = 1; c < m->cols; ++c) {
        printf(" %d", m->vals[r * m->cols + c]);
      }
      printf("\n");
    }
  }
}
matrix *allocate_matrix(int rows, int cols) {   // this allocates memory
  matrix *m = malloc(sizeof(matrix));           // for the given matrix
  if (m) {
    m->rows = rows;
    m->cols = cols;
    m->vals = malloc(sizeof(int) * rows * cols);
    if (m->vals == NULL) {
      free(m);
      m = NULL;
    }
  }
  return m;
}
matrix *read_matrix(void) {     // this reads the matrix and its size
  matrix *m = NULL;
  int rows, cols;
  if (scanf("%d %d", &rows, &cols) == 2 && rows >= 0 && cols >= 0 
  && (m = allocate_matrix(rows, cols))) {
    int *vals = m->vals;
    for (int i = 0; i < rows * cols; ++i) {
      if (scanf("%d", vals++) != 1) {
        deallocate_matrix(&m);
        break;
      }
    }
  }
  return m;
}
void deallocate_matrix(matrix **m) {    // this frees up memory
  if (m && *m) {
    if ((*m)->vals) {
      free((*m)->vals);
    }
    free(*m);
    *m = NULL;
  }
}
char read_operator(int* ret) {      // this reads the operator
  char op[2] = {'\0'};
  if (scanf("%1s", op) != 1 || 
  !(op[0] == '-' || op[0] == '+' || op[0] == '*')) {
    *ret = ERROR_INPUT;
  }
  return op[0];
}
// this below performs addition or subtraction of given matrices
matrix *sum(const matrix *const m1, const matrix *const m2, int sign) {
  matrix *result = NULL;
  if (m1 && m1->vals && m2 && m2->vals && m1->rows == m2->rows && 
  m1->cols == m2->cols && (result = allocate_matrix(m1->rows, m1->cols))) {
    for (int i = 0; i < m1->rows * m1->cols; ++i) {
      result->vals[i] = m1->vals[i] + sign * m2->vals[i];
    }
  }
  return result;
}
// this below performs multiplication of given matrices
matrix *multiplicate(const matrix *const m1, const matrix *const m2) {
  matrix *result = NULL;
  if (m1 && m1->vals && m2 && m2->vals && m2->rows == m1->cols 
  && (result = allocate_matrix(m1->rows, m2->cols))) {
    for (int r = 0; r < m1->rows; ++r) {
      for (int c = 0; c < m2->cols; ++c) {
        int idxre = r * m2->cols + c;
        result->vals[idxre] = 0;
        for (int i = 0; i < m2->rows; ++i) {
          int idxm1 = r * m1->cols + i;
          int idxm2 = i * m2->cols + c;
          result->vals[idxre] += m1->vals[idxm1] * m2->vals[idxm2];
        }
      }
    }
  }
  return result;
}
