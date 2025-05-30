#include "read_calc.h"

#define INIT_SIZE 10

number_t* read_number(char* str) {
   number_t* tmp = (number_t*)malloc(sizeof(number_t));
   if (!tmp)
      return NULL;
   memset(tmp->number, false, NUMBER_LENGTH);
   tmp->operator = ' ';
   size_t length = strlen(str);
   for (size_t i = 0; i < length; ++i)
      if (str[i] == '1' || str[i] == '0') {
	 tmp->number[NUMBER_LENGTH - (length - i)] = str[i] == '1';
      } else 
	 return NULL;
   return tmp;
}

expression_t* read_expression(char* input) {
   expression_t* tmp = (expression_t*)malloc(sizeof(expression_t));
   if (!tmp)
      return NULL;
   tmp->expr_size = INIT_SIZE;
   tmp->numbers = (number_t**)malloc(sizeof(number_t*) * tmp->expr_size);
   if (!tmp->numbers)
      return NULL;
   char str[NUMBER_LENGTH];
   memset(str, 0, NUMBER_LENGTH);
   size_t idx = 0, i = 0, ii = 0;
   size_t l = 0;
   char op = ' ';
   while (input[i] != '\0') {
      switch (input[i]) {
	 case '0' : 
	 case '1' : str[ii++] = input[i]; break;
	 case ' ' : l = ii; break;
	 case '+' :
	 case '-' :
	 case '*' :
	 case '/' : l = 1; op = input[i]; break;
      }
      i++;
      if (input[i] == '\0')
	 l = ii;
      if (l > 1) {
	 if (idx == tmp->expr_size) {
	    number_t** t = (number_t**)realloc(tmp->numbers, sizeof(number_t*) * tmp->expr_size * 2);
	    if (!t) {
	       free(tmp->numbers);
	       free(tmp);
	       return NULL;
	    }
	    tmp->numbers = t;
	    tmp->expr_size *= 2;                
	 }
	 tmp->numbers[idx++] = read_number(str);
	 memset(str, 0, NUMBER_LENGTH);
	 ii = 0;
      }
      if (l == 1 && (op == '+' || op == '-' || op == '*' || op == '/') && idx > 0)
	 tmp->numbers[idx - 1]->operator = op;
      l = 0;
   }
   if (idx < tmp->expr_size) {
      number_t** t = (number_t**)realloc(tmp->numbers, sizeof(number_t*) * idx);
      if (!t) {
	 free(tmp->numbers);
	 free(tmp);
	 return NULL;
      }
      tmp->numbers = t;
      tmp->expr_size = idx;
   }
   return tmp;
}

number_t* create_copy(number_t *num) {
   number_t* tmp = (number_t*)malloc(sizeof(number_t));
   if (!tmp)
      return NULL;
   tmp->operator = num->operator;
   for (size_t i = 0; i < NUMBER_LENGTH; ++i)
      tmp->number[i] = num->number[i];
   return tmp;
}

void delete_number(number_t *num) {
   if (num)
      free(num);
}

void delete_exppression(expression_t *expr) {
   if (expr) {
      for (size_t i = 0; i < expr->expr_size; ++i) 
	 delete_number(expr->numbers[i]);
      free(expr->numbers);
      free(expr);
   }
}
