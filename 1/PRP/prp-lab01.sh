#!/bin/bash

my_dir=prp/lab01
mkdir -p $my_dir
touch $my_dir/prp-lab01.c
tree

mkdir cvut-fel
mv prp cvut-fel
mv cvut-fel/$my_dir/prp-lab01.c cvut-fel/$my_dir/main.c
tree
