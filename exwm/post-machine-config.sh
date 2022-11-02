
THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

### common config ###
# do something here

### machine specific config ###
OPTION_PATH="${DIR_PATH}/option.txt"
if [ -f $OPTION_PATH ]; then
    OPTION_VALUE=$(cat $OPTION_PATH)
    if [ $OPTION_VALUE = "laptop" ]; then
        :  # pass
    elif [ $OPTION_VALUE = "descartes" ]; then
        # xmodmap $DIR_PATH/dependent/ctrl-caps-swap.xmodmap

        # # Adjust screen off time
        # xset dpms 0 0 1800  # 1800 secs = 30 mins
        xset s 1800 1800
        # :  # pass
    elif [ $OPTION_VALUE = "something-other" ]; then
        :  # pass
    fi
fi
