


;;; conditional load
;; http://crazia.tistory.com/entry/Emacs-%EC%82%AC%EC%A0%84-%EC%93%B0%EA%B8%B0-dictionary-%ED%8C%A8%ED%82%A4%EC%A7%80-%EC%84%A4%EC%B9%98%ED%95%98%EA%B8%B0

;; (condition-case nil  ; http://stackoverflow.com/questions/7790382/how-to-determine-whether-a-package-is-installed-in-elisp
;;     (progn
;;       (load "dictionary-init")

;;       ;; dictionary key bindings
;;       (global-set-key "\C-cs" 'dictionary-search)
;;       (global-set-key "\C-cm" 'dictionary-match-words))
;;   ;(file-error (message "Org not available; not configuring") ))
;; )

(catch 'dict-path
  (dolist (path load-path)
    (if (string= (car (last (split-string path "/")))
		 "dictionary-1.10")
	(progn  ; then
	  (load "dictionary-init")
	  ;; dictionary key bindings
	  (global-set-key "\C-cs" 'dictionary-search)
	  (global-set-key "\C-cm" 'dictionary-match-words)
	  (throw 'dict-path t)))
    (progn nil)  ; else
    )
  )

