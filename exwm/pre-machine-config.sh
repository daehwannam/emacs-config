
SOURCING=false

if $SOURCING; then
    DIR_PATH=/home/dhnam/.emacs.d/exwm
else
    THIS_PATH=`realpath $0`
    DIR_PATH=`dirname $THIS_PATH`
fi

OPTION_PATH="${DIR_PATH}/option.txt"
if [ -f $OPTION_PATH ]; then
    OPTION_VALUE=$(cat $OPTION_PATH)
    if [ $OPTION_VALUE = "laptop" ]; then
        xinput set-prop "ETPS/2 Elantech Touchpad" "libinput Tapping Enabled" 1  # touchpad setting
    elif [ $OPTION_VALUE = "descartes" ]; then
        # xmodmap $DIR_PATH/dependent/ctrl-caps-swap.xmodmap

        # Adjust screen off time
        xset dpms 0 0 1800  # 30 mins
        :  # pass
    elif [ $OPTION_VALUE = "something-other" ]; then
        :  # pass
    fi
fi
