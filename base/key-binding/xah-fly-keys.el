
(dhnam/install-package-unless-installed 'xah-fly-keys)

(progn
  (require 'xah-fly-keys)

  ;; specify a layout
  (xah-fly-keys-set-layout "qwerty")

  ;; possible values
  ;; adnw , azerty , azerty-be , beopy , bepo , carpalx-qfmlwy , carpalx-qgmlwb , carpalx-qgmlwy , colemak , colemak-dhm , colemak-dhm-angle , colemak-dhk , dvorak , koy , neo2 , norman , programer-dvorak , pt-nativo , qwerty , qwerty-abnt , qwerty-no (qwerty Norwegian) , qwertz , workman

  (xah-fly-keys 1))
