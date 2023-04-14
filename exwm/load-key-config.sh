#!/usr/bin/sh

THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

# xmodmap keycode example
# https://github.com/jonathf/dotfiles/blob/master/.Xmodmap

OPTION_PATH="${DIR_PATH}/option.txt"
if [ -f $OPTION_PATH ]; then
    OPTION_VALUE=$(cat $OPTION_PATH)
    if [ $OPTION_VALUE = "laptop" ]; then
        # Left Alt = <XF86WWAN>
        xmodmap  -e 'keycode 254 = XF86WWAN'
        # xcape -t 200 -e 'Control_L=CruzeiroSign'
        xcape -t 200 -e 'Alt_L=XF86WWAN'
        # :  # pass
    elif [ $OPTION_VALUE = "descartes" ]; then
        # xmodmap $DIR_PATH/dependent/ctrl-caps-swap.xmodmap
        :  # pass
    elif [ $OPTION_VALUE = "something-other" ]; then
        # Left Alt = ₢
        xmodmap  -e 'keycode 254 = CruzeiroSign'
        # xcape -t 200 -e 'Control_L=CruzeiroSign'
        xcape -t 200 -e 'Alt_L=CruzeiroSign'
        # :  # pass
    fi
fi

# ₣
# xmodmap  -e 'keycode 255 = FFrancSign'
# xcape -t 200 -e 'Shift_R=FFrancSign'
# # xcape -t 200 -e 'Shift_L=FFrancSign'

# ₫
# xmodmap  -e 'keycode 253 = DongSign'
# xcape -t 200 -e 'Control_L=DongSign'
# xcape -t 200 -e 'Shift_R=DongSign'

# # ₤
# xmodmap  -e 'keycode 252 = LiraSign'
# xcape -t 200 -e 'Shift_L=LiraSign'
