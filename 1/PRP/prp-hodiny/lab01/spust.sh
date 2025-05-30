#!/bin/bash

clang -Wall -pedantic -std=c99 -g main.c -o main
IS_OK=`echo $?`

echo "IS_OK=$IS_OK"
if [[ "$IS_OK" -eq 0 ]]
then
    ./main
fi