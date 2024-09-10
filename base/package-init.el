(require 'dhnam-package)

(dhnam/package-initialize
 '(("melpa" . "https://melpa.org/packages/")
   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
   ;; ("gnu-devel" "https://elpa.gnu.org/devel/")
   ))

(progn
  (dhnam/install-package-unless-installed 'use-package)
  (require 'use-package))

(progn
  (dhnam/install-package-unless-installed 'quelpa)

  (progn
    ;; disabling auto-updating
    (setq quelpa-update-melpa-p nil)))
