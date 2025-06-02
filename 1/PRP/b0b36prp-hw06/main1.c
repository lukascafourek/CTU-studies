#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define ERROR_INPUT 100
#define EXIT_SUCCESS 0

void print_matrix(int rows, int cols, int matrix[rows][cols]);
bool sum(
  int rows1, int cols1, int matrix1[rows1][cols1],
  int rows2, int cols2, int matrix2[rows2][cols2],
  int rows3, int cols3, int matrix3[rows3][cols3],
  int sign);
bool multiplicate(
  int rows1, int cols1, int matrix1[rows1][cols1],
  int rows2, int cols2, int matrix2[rows2][cols2],
  int rows3, int cols3, int matrix3[rows3][cols3]);
int check_error(bool ok, int ret);
bool operator(char* op);
bool matrixes(int rows, int cols, int matrix[rows][cols], bool ok);
bool calculate(
  int rows1, int cols1, int matrix1[rows1][cols1], 
  int rows2, int cols2, int matrix2[rows2][cols2],
  char* op, bool ok);

int main(int argc, char *argv[]) {
  _Bool ok = true;
  int ret = EXIT_SUCCESS, rows1, cols1;
  if (scanf("%d %d", &rows1, &cols1) == 2 && rows1 >= 0 && cols1 >= 0) {
    int m1[rows1][cols1];
    ok = matrixes(rows1, cols1, m1, ok);
    if (!ok)
      goto end;
    char op[2];
    ok = operator(op);
    if (!ok)
      goto end;
    int rows2, cols2;
    if (scanf("%d %d", &rows2, &cols2) == 2 && rows2 >= 0 && cols2 >= 0) {
      int m2[rows2][cols2];
      ok = matrixes(rows2, cols2, m2, ok);
      if (!ok)
        goto end;
    ok = calculate(rows1, cols1, m1, rows2, cols2, m2, op, ok);
    } else
      ok = false;
  } else
    ok = false;
end:
  ret = check_error(ok, ret);
  return ret;
}
int check_error(bool ok, int ret) {     // this checks for error
  if (!ok) {
    fprintf(stderr, "Error: Chybny vstup!\n");
    ret = ERROR_INPUT;
  }
  return ret;
}
bool matrixes(int rows, int cols, int matrix[rows][cols], bool ok) {
  for (int r = 0; r < rows; ++r) {      // this scans given matrix (input)
    for (int c = 0; c < cols; ++c) {
      if (scanf("%d", &matrix[r][c]) != 1)
        ok = false;
      else
        ok = true;
    }
  }
  return ok;
}
bool operator(char* op) {     // this scans given operator (input)
  if (scanf("%1s", op) != 1 || 
  !(op[0] == '-' || op[0] == '+' || op[0] == '*'))
    return false;
  else
    return true;
}
bool calculate(   // this performs calculation functions based on the operator
  int rows1, int cols1, int matrix1[rows1][cols1], 
  int rows2, int cols2, int matrix2[rows2][cols2],
  char* op, bool ok) {
  int sign = 1;
  switch(op[0]) {
    case '-':
      sign = -1;
    case '+':
    {
      int result[rows1][cols1];
      ok = sum(rows1, cols1, matrix1, rows2, cols2, matrix2, 
               rows1, cols1, result, sign);
      if (ok) {
        print_matrix(rows1, cols1, result);
      }
      break;
    }
    case '*':
    {
      int product[rows1][cols2];
      ok = multiplicate(rows1, cols1, matrix1, rows2, cols2, matrix2, 
                        rows1, cols2, product);
      if (ok) {
        print_matrix(rows1, cols2, product);
      }
      break;
    }
  }
  return ok;
}
bool sum(     // this adds or substracts given matrixes based on the operator
  int rows1, int cols1, int matrix1[rows1][cols1],
  int rows2, int cols2, int matrix2[rows2][cols2],
  int rows3, int cols3, int matrix3[rows3][cols3],
  int sign) {
  if (rows1 == rows2 && cols1 == cols2 && rows1 == rows3 && cols1 == cols3) {
    for (int r = 0; r < rows1; ++r) {
      for (int c = 0; c < cols1; ++c)
        matrix3[r][c] = matrix1[r][c] + sign * matrix2[r][c];
    }
    return true;
  } else
    return false;
}
bool multiplicate(    // this multiplies given matrixes
  int rows1, int cols1, int matrix1[rows1][cols1],
  int rows2, int cols2, int matrix2[rows2][cols2],
  int rows3, int cols3, int matrix3[rows3][cols3]) {
  if (rows2 == cols1 && rows1 == rows3 && cols2 == cols3) {
    for (int r = 0; r < rows1; ++r) {
      for (int c = 0; c < cols2; ++c) {
        matrix3[r][c] = 0;
        for (int i = 0; i < rows2; ++i)
          matrix3[r][c] += matrix1[r][i] * matrix2[i][c];
      }
    }
    return true;
  } else
    return false;
}
void print_matrix(int rows, int cols, int matrix[rows][cols]) {
  printf("%d %d\n", rows, cols);        // this prints the result
  for (int r = 0; r < rows; ++r) {
    printf("%d", matrix[r][0]);
    for (int c = 1; c < cols; ++c)
      printf(" %d", matrix[r][c]);
    printf("\n");
  }
}
