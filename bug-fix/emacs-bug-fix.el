

(when (version< emacs-version "30.0")
  (with-eval-after-load 'hangul
    ;; Fix for `hangul-insert-character' about `self-insert-command'.
    ;;
    ;; Original Source:
    ;; /usr/share/emacs/27.1/lisp/leim/quail/hangul.el.gz
    ;;
    ;; Related:
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66970
    ;; https://github.com/emacs-mirror/emacs/commit/8090ab05434f39b73e6238ebc5ab8e4fcc52acf3

    (defun-override hangul-insert-character (&rest queues)
      "Insert characters generated from QUEUES.
Each queue has the same form as `hangul-queue'.
Setup `quail-overlay' to the last character."
      (if (and mark-active transient-mark-mode)
          (progn
            (delete-region (region-beginning) (region-end))
            (deactivate-mark)))
      (let* ((chars-to-insert
              (with-temp-buffer
                (dolist (queue queues (mapcar #'identity (buffer-string)))
                  (insert
                   (hangul-character
                    (+ (aref queue 0) (hangul-djamo 'cho (aref queue 0) (aref queue 1)))
                    (+ (aref queue 2) (hangul-djamo 'jung (aref queue 2) (aref queue 3)))
                    (+ (aref queue 4) (hangul-djamo 'jong (aref queue 4) (aref queue 5))))))))
             (overwrite-maybe
              (or
               ;; If the overlay isn't showing (i.e. it has 0 length) then
               ;; we may want to insert char overwriting (iff overwrite-mode is
               ;; non-nil, of course)
               (= (overlay-start quail-overlay) (overlay-end quail-overlay))
               ;; Likewise we want to do it if there is more then one
               ;; character that were combined.
               (cdr chars-to-insert))))
        (quail-delete-region)             ; this empties the overlay
        (dolist (c chars-to-insert)
          (let ((last-command-event c)
                (overwrite-mode (and overwrite-mode
                                     overwrite-maybe
                                     overwrite-mode)))
            (self-insert-command 1)
            ;; For chars other than fhe first, no more overwrites desired
            (setq overwrite-maybe nil)))
                                        ; this shows the overlay again (TODO: do we really always revive?)
        (move-overlay quail-overlay (1- (point)) (point))))))

