#!/usr/bin/bash

if [ ! $1 ]; then
    # TTY_NUM=$(basename $(tty))
    TTY_NAME=$(tty)
    TTY_NUM="${TTY_NAME: -1}"  # pick the last char

fi

# startx -display :$DISPLAY  ~/.emacs.d/exwm/xinitrc.sh vt$TTY_NUM

startx ~/.emacs.d/exwm/xinitrc.sh -- vt$TTY_NUM
