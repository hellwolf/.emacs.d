;;; -*- lexical-binding: t -*-
(require 'browse-url)
(require 'evil-core)
(require 'my-lib)


;; Overide I=initial evil states for some major modes
(evil-set-initial-state 'Buffer-menu-mode 'motion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WSL2 Specifics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string-match-p "-WSL2\\'" operating-system-release)
  (message "On WSL2, using wslviewpath for browse-url")
  (setq
    browse-url-generic-program (my-catfile my-tools-path "wslviewpath")
    browse-url-generic-args '()
    browse-url-browser-function #'browse-url-generic))

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:
