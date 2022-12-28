

;; making TAGS
;; https://www.emacswiki.org/emacs/BuildTags#toc3
;;
;; command: M-x compile RET find . -type f -iname "*.el" | etags - RET


;; setting tags table
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html

(when (dhnam/machine-config-get-first 'etags-list)
  (setq tags-table-list (dhnam/machine-config-get-first 'etags-list)))

(provide 'dhnam-tags)
