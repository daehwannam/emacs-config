
(when (package-installed-p 'minibuffer-line)
  ;; https://emacs.stackexchange.com/a/19856
  (require 'minibuffer-line)
  (setq minibuffer-line-format '((:eval
                                  (let ((time-string (format-time-string "%l:%M %b %d %a")))
                                    (concat
                                     (make-string (- (frame-text-cols)
                                                     (string-width time-string)) ? )
                                     time-string)))))
  (minibuffer-line-mode))
