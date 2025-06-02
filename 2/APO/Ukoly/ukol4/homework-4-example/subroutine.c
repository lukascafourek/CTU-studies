/* Read number and produce "data" of given length */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "subroutine.h"

int subroutine_fnc(int fd, int numeric_base)
{
  int val = 0;
  int err_res;
  int res;
  char ch;

  do {

    res = read(fd, &ch, 1);
    if (res != 1)
      exit(1);

    if((ch<'0') || (ch>='0'+numeric_base))
      break;

    val *= numeric_base;
    val += ch - '0';
  } while(1);

  return val;
}


