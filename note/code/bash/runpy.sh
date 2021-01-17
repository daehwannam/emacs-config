#!/bin/bash

# execute "chmod a+x runpy.sh" for convenience

argv=("$@")
path=${argv[0]}

py_module=${path//\//.}  # replace '/' with '.'
py_module=$(basename $py_module .py)
py_args=${argv[@]:1}

python -m $py_module ${py_args[@]}
