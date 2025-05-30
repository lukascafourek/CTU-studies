#ifndef __CALC_H__
#define __CALC_H__

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "read_calc.h"

/* prints the number */
void print_number(number_t *num);

/* returns true if num1 == num2; false otherwise */
bool is_equal(number_t *num1, number_t *num2);

/*
 * calculates the sum, difference, product, quotient of num1 and num2
 * stores the result in num1
 */
void sum(number_t *num1, number_t *num2);
void difference(number_t *num1, number_t *num2);
void product(number_t *num1, number_t *num2);
void quotient(number_t *num1, number_t *num2);

/* evaluates the expression expr and returns the result */
number_t* expression_evaluation(expression_t* expr);

#endif /* __CALC_H__ */
