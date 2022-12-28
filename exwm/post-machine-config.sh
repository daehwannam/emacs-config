
THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

### common config ###

# ₣
xmodmap  -e 'keycode 255 = FFrancSign'
xcape -t 200 -e 'Shift_R=FFrancSign'

# ₢
xmodmap  -e 'keycode 254 = CruzeiroSign'
xcape -t 175 -e 'Alt_L=CruzeiroSign'
# xcape -t 175 -e 'Control_L=CruzeiroSign'

# # ₤
# xmodmap  -e 'keycode 250 = LiraSign'
# xcape -t 200 -e 'Shift_R=LiraSign'


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
