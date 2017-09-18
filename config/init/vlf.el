;;; elpa : package manage tool
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;;; vlf setting
;; https://github.com/m00natic/vlfi
(if (boundp 'vlf-setup)
    (require 'vlf-setup))
