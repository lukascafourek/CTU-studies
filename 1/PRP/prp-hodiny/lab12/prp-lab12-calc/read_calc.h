#ifndef __READ_CALC_H__
#define __READ_CALC_H__

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUMBER_LENGTH 50
/* Number structure which holds all necessary data */
typedef struct {
   bool number[NUMBER_LENGTH];
   char operator;
} number_t;

typedef struct {
   number_t** numbers;
   size_t expr_size;
} expression_t;

/* reads a new number */
number_t* read_number(char* str);

/* reads a expession */
expression_t* read_expression(char* input);

/* creates a copy of number num and returns this copy */
number_t* create_copy(number_t *num);

/* deletes the number */
void delete_number(number_t *num);

/* deletes the expression and all allocated memory */
void delete_exppression(expression_t *expr);

#endif /* __READ_CALC_H__ */

