#!/usr/bin/sh

# ₣
xmodmap  -e 'keycode 255 = FFrancSign'
xcape -t 200 -e 'Shift_R=FFrancSign'

# ₢
xmodmap  -e 'keycode 254 = CruzeiroSign'
xcape -t 200 -e 'Alt_L=CruzeiroSign'
# xcape -t 175 -e 'Control_L=CruzeiroSign'

# ₫
xmodmap  -e 'keycode 253 = DongSign'
xcape -t 200 -e 'Control_L=DongSign'

# # ₤
# xmodmap  -e 'keycode 250 = LiraSign'
# xcape -t 200 -e 'Shift_R=LiraSign'
