
(when (display-graphic-p)
  (require 'dhnam-gui-term)
  (key-chord-define-global "o3" 'dhnam/gui-terminal))


(provide 'init-gui-term)
