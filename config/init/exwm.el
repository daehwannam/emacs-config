(let ((exwm-enabled (machine-config-get-first 'exwm-enabled)))
  (when exwm-enabled
   (defun exwm-simple-init ()
     (progn
       ;; disable scroll-bar, menu-bar, tool-bar
       (toggle-scroll-bar -1)
       (menu-bar-mode -1)
       (tool-bar-mode -1))
     (progn
       (require 'exwm)
       (require 'exwm-config)))

   (defun exwm-config-mine ()
     (exwm-simple-init)
     (progn
       (exwm-config-example)          ; same with (exwm-config-default)
       (ido-mode nil))
     )
   (exwm-config-mine)))
