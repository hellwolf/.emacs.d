(require 'browse-url)
(require 'my-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WSL2 Specifics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string-match-p "-WSL2\\'" operating-system-release)
  (message "On WSL2, using wslviewpath for browse-url")
  (setq
    browse-url-generic-program (my-catfile my-tools-path "wslviewpath")
    browse-url-generic-args '()
    browse-url-browser-function #'browse-url-generic))
