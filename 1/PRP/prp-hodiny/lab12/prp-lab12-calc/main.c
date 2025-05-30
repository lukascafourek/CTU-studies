#include "stdio.h"
#include "stdlib.h"

#include "calc.h"
#include "read_calc.h"


/*
 * TEST PROGRAM
 * - reads commands from stdin and executes them in the queue
 */
int main(int argc, char *argv[])
{
   expression_t* expr;
   expr = read_expression("1010111");
   print_number(expr->numbers[0]);
   number_t* num = read_number("1010111");
	if (is_equal(expr->numbers[0], num))
	   printf("Cisla jsou stejna.\n");
	else
	   printf("Cisla jsou rozdilna.\n");
   delete_number(num);
   delete_exppression(expr);
   expression_t* expr_control;
   expr = read_expression("1010111 + 1011");
   expr_control = read_expression("1100010");
   print_number(expr->numbers[0]);
   print_number(expr->numbers[1]);
   sum(expr->numbers[0], expr->numbers[1]);
   //difference(expr->numbers[0], expr->numbers[1]);
   print_number(expr->numbers[0]);
   if (is_equal(expr->numbers[0], expr_control->numbers[0]))
      printf("Soucet je OK\n");
   delete_exppression(expr_control);
   delete_exppression(expr);
   return EXIT_SUCCESS;
}
