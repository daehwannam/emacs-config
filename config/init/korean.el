
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
 ((string-equal system-type "windows-nt") ; Microsoft Windows
 ;; ((string-equal machine-domain "ms") ; Microsoft Windows
  (progn
    ;;; korean language setting
    ;; http://devnauts.tistory.com/51
    (set-language-environment "Korean")
    (prefer-coding-system 'utf-8)
    (set-file-name-coding-system 'cp949-dos)
    (global-unset-key (kbd "S-SPC"))

    (comment
     ;; https://libsora.so/posts/emacs-hangul-key/
     ;; <kana>: 한/영 키
     (global-set-key (kbd "<kana>") 'toggle-input-method))))
 ((string-equal system-type "gnu/linux") ; linux
 ;; ((string-equal machine-domain "vbox") ; vbox linux
  (progn
    )))

(progn
  ;; Preventing delay for composing Korean character 
  ;; https://injae.github.io/ide/2018/11/25/%EC%9D%B4%EB%A7%A5%EC%8A%A4%EB%82%B4%EC%9E%A5%ED%95%9C%EA%B8%80%EC%9E%85%EB%A0%A5%EA%B8%B0%EC%82%AC%EC%9A%A9%EB%B2%95/

  ;; 두벌식 키보드 설정
  (setq default-korean-keyboard 'korean-hangul2)
  (unless (assoc (symbol-name 'korean-hangul2) input-method-alist)
    (setq default-input-method 'korean-hangul))
  ;; 아래 설정은 한영 전환을 shift-tab으로 설정하는 설정입니다.
  (global-set-key (kbd "S-<SPC>") 'toggle-input-method))
