
;; read file contents
;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))



;; check OS type
;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html

(cond
; ((string-equal system-type "windows-nt") ; Microsoft Windows
 ((string-equal machine-domain "ms") ; Microsoft Windows
  (progn


    ;;; korean language setting
    ;; http://devnauts.tistory.com/51
    (set-language-environment "Korean")
    (prefer-coding-system 'utf-8)
    (set-file-name-coding-system 'cp949-dos)
    (global-unset-key (kbd "S-SPC"))

    ;; https://libsora.so/posts/emacs-hangul-key/
    (global-set-key (kbd "<kana>") 'toggle-input-method) ;한/영 키
    ))
; ((string-equal system-type "gnu/linux") ; linux
 ((string-equal machine-domain "vbox") ; vbox linux
  (progn
    )))



