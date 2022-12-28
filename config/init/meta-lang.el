
(progn
  ;; OCaml
  ;; https://stackoverflow.com/q/25934406
  ;; 
  ;; getting path
  ;; # opam switch 4.01.0
  ;; # eval $(opam env)
  ;;
  (let ((bin-path "/home/dhnam/.opam/4.01.0/bin"))
    (comment (setq exec-path (append exec-path (list bin-path))))
    (add-to-list 'exec-path bin-path)))

(provide 'dhnam-meta-lang)
