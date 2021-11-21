
(let ((eaf-path (machine-config-get-first 'eaf-path)))
  (when eaf-path
    (add-to-list 'load-path eaf-path)
    (require 'eaf)
    (require 'eaf-browser)
    (require 'eaf-pdf-viewer)))
