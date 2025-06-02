/* Read number and produce "data" of given length */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "subroutine.h"

char out_file_name[] = "data";

int toplevel_fnc(void)
{
  int ret_val;
  int err_res;
  int count;
  int result;
  char ch = 'A';
  int fd;

  count = subroutine_fnc(0, 10);

  result = count;

                                                         /* 01102 */
  fd = open(out_file_name, O_RDWR|O_CREAT|O_TRUNC , S_IRUSR|S_IWUSR);
  if(fd<0)
    exit(1);

  while(count--) {
    ret_val = write(fd, &ch, 1);
    if(ret_val<0)
      exit(1);
  }

  ret_val = close(fd);
  if(ret_val<0)
    exit(1);

  return result;
}
