#!/bin/bash

# execute "chmod a+x runhy.sh" for convenience

argv=("$@")
path=${argv[0]}

py_module=${path//\//.}  # replace '/' with '.'
py_module=$(basename $py_module .hy)
py_args=${argv[@]:1}

hy -m $py_module ${py_args[@]}
