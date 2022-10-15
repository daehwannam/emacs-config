
(progn
  ;; Enable tramp to use PATH of remote environments 
  ;; https://stackoverflow.com/a/61169654
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
