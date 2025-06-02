/* Implementation of the subroutine subroutine_fnc */

#include "subroutine.h"

int optional_char_to_skip = 0;

int subroutine_fnc(int *var, int input_char)
{
    if (input_char == 32 || input_char == 9 || input_char == optional_char_to_skip) {
        int temp = *var;
        *var = temp + 1;
        if (temp != 0) {
            return 0;
        }
    } else {
        *var = 0;
    }
    return input_char;
}
