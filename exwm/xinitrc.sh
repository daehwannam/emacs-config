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

# Common configuration
xset r rate 250 50

# Run custom script
if [ -f "~/.exwm-config"]; then
    bash ~/.exwm-config
fi

# Finally start Emacs
# export BASH_ENV=~/.bashrc  # not working

exec emacs --exwm
# exec emacs --debug-init --exwm
