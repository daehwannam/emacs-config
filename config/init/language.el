(progn
  ;; Korean setup

  (cond
   ;; check OS type
   ;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html

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
    ;; for linux
    (global-set-key (kbd "<Hangul>") 'toggle-input-method))

  (progn
    ;; Preventing delay for composing Korean character 
    ;; https://injae.github.io/ide/2018/11/25/%EC%9D%B4%EB%A7%A5%EC%8A%A4%EB%82%B4%EC%9E%A5%ED%95%9C%EA%B8%80%EC%9E%85%EB%A0%A5%EA%B8%B0%EC%82%AC%EC%9A%A9%EB%B2%95/

    ;; 두벌식 키보드 설정
    (setq default-korean-keyboard 'korean-hangul2)

    ;; set the default input method
    (unless (assoc (symbol-name 'korean-hangul2) input-method-alist)
      (setq default-input-method 'korean-hangul))

    ;; 아래 설정은 한영 전환을 shift-tab으로 설정하는 설정입니다.
    (global-set-key (kbd "S-<SPC>") 'toggle-input-method))
  )

(progn
  ;; Font setup for org-mode tables in GUI
  ;; https://stackoverflow.com/a/3958530
  ;;
  ;; Installing Inconsolata font for Ubuntu:
  ;; $ sudo apt-get install fonts-inconsolata

  (when (find-font (font-spec :name "Inconsolata-12"))
    (progn
      ;; https://emacs.stackexchange.com/a/16314
      (set-face-font 'default "Inconsolata-12")

      (when (and (boundp 'dhnam/exwm-cmd-arg-passed) dhnam/exwm-cmd-arg-passed)
        (let ((font-height (dhnam/machine-config-get-first 'exwm-font-height)))
          (when font-height
            ;; font size
            (set-face-attribute 'default nil :height font-height)
            (progn
              ;; some themes, such as manoj-dark, changes font-height to a fixed value
              (comment (set-face-attribute 'completions-common-part nil :height (round (* font-height 1.0))))
              (set-face-attribute 'completions-common-part nil :height 'unspecified))))))
    (comment
     ;; this make non full-sized frames for exwm
     (defun dhnam/make-frame-command-font-advice (&rest args)
       (set-frame-font "Inconsolata-12"))
     (advice-add 
      'make-frame-command
      :after
      'dhnam/make-frame-command-font-advice
      '((name . "dhnam/make-frame-command-font-advice")))
     (progn
       ;; for thee fist frame
       (dhnam/make-frame-command-font-advice)))))

(defun dhnam/toggle-caps-lock-key ()
  (interactive)
  (start-process-shell-command "xte" nil "xte 'key Caps_Lock'"))
