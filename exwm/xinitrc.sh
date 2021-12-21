#!/usr/bin/sh

# Disable access control for the current user.
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Set default cursor.
xsetroot -cursor_name left_ptr

# Set keyboard repeat rate.
xset r rate 200 60

# Uncomment the following block to use the exwm-xim module.
#export XMODIFIERS=@im=exwm-xim
#export GTK_IM_MODULE=xim
#export QT_IM_MODULE=xim
#export CLUTTER_IM_MODULE=xim

#
# Below is the modifed code from "/home/$USER/.emacs.d/elpa/exwm-0.26/xinitrc"
#

### config for each machine ###
THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

source $DIR_PATH/pre-machine-config.sh

### common configuration ###

# fcitx setting
# https://wiki.archlinux.org/title/fcitx#Set_environment_variables_for_IM_modules
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx &  # run fcitx daemon

# Set keyboard repeat rate
xset r rate 250 50

# run custom script
if [ -f "~/.exwm-config"]; then
    bash ~/.exwm-config
fi

### run emacs ###
# export BASH_ENV=~/.bashrc  # not working

exec emacs --exwm
# exec emacs --debug-init --exwm
