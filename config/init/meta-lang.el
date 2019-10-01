
;; OCaml
;; https://stackoverflow.com/q/25934406
;; 
;; getting path
;; # opam switch 4.01.0
;; # eval $(opam env)
;; 
(setq exec-path (append exec-path (list "/home/dhnam/.opam/4.01.0/bin")))
