/* Implementation of the subroutine toplevel_fnc */

#include "subroutine.h"

int toplevel_fnc(void)
{
    int buffer;
    int var = 0;
    int count = 0;
    while (read(0, &buffer, 1) == 1) {
        int result = subroutine_fnc(&var, buffer);
        if (result == 0) {
            count++;
            continue;
        }
        write(1, &result, 1);
    }
    return count;
}
