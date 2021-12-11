#!/usr/bin/sh

if [ ! $1 ]; then
    TTY_NUM=$(basename $(tty))
fi

# startx -display :$DISPLAY  ~/.emacs.d/exwm/xinitrc.sh vt$TTY_NUM

startx ~/.emacs.d/exwm/xinitrc.sh vt$TTY_NUM
