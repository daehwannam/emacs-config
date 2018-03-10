;;; load theme.el
(load "~/.emacs.d/config/init/theme.el")

;;; load-path setting
(package-initialize)

(defun add-to-load-path-recursively (path)
  (add-to-list 'load-path path)

  ;; recursively add packages from a directory to load-path
  ;; http://stackoverflow.com/questions/7322246/adding-subdirectories-to-load-path
  (let ((default-directory path))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-load-path-recursively "~/.emacs.d/config/bin/")
(add-to-load-path-recursively "~/.emacs.d/config/package/")
(add-to-load-path-recursively "~/.emacs.d/config/script/")

;;; load useful functions
(require 'load-directory)
(load-directory "~/.emacs.d/config/function")

;;; initialize machine id if it's not exist.
(unless (file-exists-p "~/.emacs.d/config/domain.txt")
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  (write-region "default-domain" nil "~/.emacs.d/config/domain.txt"))

(defvar machine-domain (trim-string (get-string-from-file "~/.emacs.d/config/domain.txt")))

;;; package setting
(load "~/.emacs.d/package.el")

;;; load all init files
;; https://www.emacswiki.org/emacs/LoadingLispFiles
;; (require 'load-directory)
(load-directory "~/.emacs.d/config/init")
