(let ((exwm-enabled (machine-config-get-first 'exwm-enabled)))
  (when exwm-enabled
    (progn
      ;; disable scroll-bar, menu-bar, tool-bar
      (toggle-scroll-bar -1) ; 
      (menu-bar-mode -1)
      (tool-bar-mode -1))

    (progn
      (require 'exwm)
      (require 'exwm-config)
      ;; (exwm-config-example)
      )
    ))
