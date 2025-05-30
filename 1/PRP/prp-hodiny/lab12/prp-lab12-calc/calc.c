#include "calc.h"

void print_number(number_t *num) {
    bool is_print = false;
    for (size_t i = 0; i < NUMBER_LENGTH; ++i) {
        if (num->number[i])
            is_print = true;
        if (is_print)
            printf("%d", num->number[i]);
    }
    printf("\n");
}
bool is_equal(number_t *num1, number_t *num2) {
    for (size_t i = 0; i < NUMBER_LENGTH; ++i)
        if (num1->number[i] != num2->number[i])
            return false;
    return true;       
}
void sum(number_t *num1, number_t *num2) {
    bool overflow = false;
    for (int i = NUMBER_LENGTH - 1; i >= 0; --i) {
        bool tmp = num1->number[i];
        num1->number[i] ^= num2->number[i];
        if (overflow) {
            num1->number[i] = !num1->number[i];
            overflow = tmp | num2->number[i];
        } else
            overflow = tmp & num2->number[i];

    }
}
void difference(number_t *num1, number_t *num2) {
    number_t *tmp = create_copy(num2);
    for (size_t i = 0; i < NUMBER_LENGTH; ++i)
        tmp->number[i] = !tmp->number[i];
    number_t *one = read_number("1");
    sum(tmp, one);
    sum(num1, tmp);
    delete_number(one);
    delete_number(tmp);
}
