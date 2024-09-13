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

(progn
  ;; Suppress native-comp warnings.
  ;; https://www.reddit.com/r/emacs/comments/l42oep/comment/kd0kw5c/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ;;
  ;; e.g. ⛔ Warning (comp): example.el:...
  ;;
  (setq native-comp-async-report-warnings-errors nil))

(progn
  ;; Hide warnings
  ;; https://stackoverflow.com/a/23752552
  ;;
  (setq warning-minimum-level :emergency))
