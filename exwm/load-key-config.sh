#!/usr/bin/sh

# ₣
xmodmap  -e 'keycode 255 = FFrancSign'
xcape -t 200 -e 'Shift_R=FFrancSign'
# xcape -t 200 -e 'Shift_L=FFrancSign'

# ₢
xmodmap  -e 'keycode 254 = CruzeiroSign'
xcape -t 200 -e 'Control_L=CruzeiroSign'
# xcape -t 200 -e 'Alt_L=CruzeiroSign'

# ₫
# xmodmap  -e 'keycode 253 = DongSign'
# xcape -t 200 -e 'Control_L=DongSign'
# xcape -t 200 -e 'Shift_R=DongSign'

# # ₤
# xmodmap  -e 'keycode 252 = LiraSign'
# xcape -t 200 -e 'Shift_L=LiraSign'
