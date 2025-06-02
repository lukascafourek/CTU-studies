#include <stdio.h>
#include "testbench.h"

char new_line = '\n';

int orders[] = {
  1000000000,
  100000000,
  10000000,
  1000000,
  100000,
  10000,
  1000,
  100,
  10,
  1,
  0
};

int main(int argc, char* argv[])
{
  int result;
  char *banner = "Starting...\n";
  char *footer = "toplevel_fnc returns ";
  int err_res;
  int *order;

  while(*banner) {
    INTERNAL_SYSCALL(write, err_res, 3, 1, banner++, 1);
  }

  result=toplevel_fnc();

  while(*footer) {
    INTERNAL_SYSCALL(write, err_res, 3, 1, footer++, 1);
  }
  order = orders;
  while ((*order > result) && (*order > 1))
    order++;
  do {
    char ch = result / *order + '0';
    result = result % *order;
    INTERNAL_SYSCALL(write, err_res, 3, 1, &ch, 1);
  } while(*(++order));
  INTERNAL_SYSCALL(write, err_res, 3, 1, &new_line, 1);

  return 0;
}
