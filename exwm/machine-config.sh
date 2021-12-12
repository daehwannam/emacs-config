
THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

OPTION_PATH="${DIR_PATH}/option.txt"
if [ -f $OPTION_PATH ]; then
    OPTION_VALUE=$(cat $OPTION_PATHcat $OPTION_PATH)
    if [ $OPTION_VALUE = "laptop" ]; then
        # touchpad setting
        xinput set-prop "ETPS/2 Elantech Touchpad" "libinput Tapping Enabled" 1
        mkdir -p ~/.emacs.d/exwm/ttt
    elif [ $OPTION_VALUE = "descartes" ]; then
        :  # pass
    fi
fi
