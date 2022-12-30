
THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

### common config ###

# Triggering key
# https://wiki.linuxquestions.org/wiki/List_of_Keysyms_Recognised_by_Xmodmap

sh "${DIR_PATH}/load-key-config.sh"

### machine specific config ###
OPTION_PATH="${DIR_PATH}/option.txt"
if [ -f $OPTION_PATH ]; then
    OPTION_VALUE=$(cat $OPTION_PATH)
    if [ $OPTION_VALUE = "laptop" ]; then
        # xmodmap $DIR_PATH/dependent/ctrl-caps-swap.xmodmap

        # Adjust screen off time
        xset s 1800 1800
        xset dpms 2100 2400 2700
        # 1800 secs = 30 mins

        # Make short-pressed Ctrl behave like Escape
        # xcape -e 'Control_L=Escape'

        # :  # pass
    elif [ $OPTION_VALUE = "descartes" ]; then
        # xmodmap $DIR_PATH/dependent/ctrl-caps-swap.xmodmap

        # Adjust screen off time
        xset s 1800 1800
        xset dpms 2100 2400 2700
        # 1800 secs = 30 mins

        # Make short-pressed Ctrl behave like Escape
        # xcape -e 'Control_L=Escape'

        # :  # pass
    elif [ $OPTION_VALUE = "something-other" ]; then
        :  # pass
    fi
fi
