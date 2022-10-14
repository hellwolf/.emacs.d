(require 'use-package)
;; TODO cleanup

(use-package browse-kill-ring)

;; browse-kill-ring - 1.3
;; url: http://www.todesschaf.org/projects/bkr.html
(require 'browse-kill-ring)
(require 'browse-kill-ring+)

;; clipboard tweaks
(setq select-enable-clipboard 't)
(setq mouse-drag-copy-region nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; browse-kill-rings, M-y
(browse-kill-ring-default-keybindings)