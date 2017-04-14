
;; read file contents
;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))



;; check OS type
;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html

(setq domain_name (get-string-from-file "~/.emacs.d/config/domain.txt"))
(cond
; ((string-equal system-type "windows-nt") ; Microsoft Windows
 ((string-equal domain_name "ms") ; Microsoft Windows
  (progn


    ;;; korean language setting
    ;; http://devnauts.tistory.com/51
    (set-language-environment "Korean")
    (prefer-coding-system 'utf-8)
    (set-file-name-coding-system 'cp949-dos)
    (global-unset-key (kbd "S-SPC"))


    ))
; ((string-equal system-type "gnu/linux") ; linux
 ((string-equal domain_name "vobox") ; vbox linux
  (progn
    )))



