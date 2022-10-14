(require 'use-package)

;; https://github.com/joodland/bm
;; Visible bookmarks in buffer for GNU Emacs 26.x, 27.x
(use-package bm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (F2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Similar to https://atom.io/packages/bookmarks
(global-set-key (kbd "C-S-<f2>") 'bm-toggle)
(global-set-key (kbd "M-S-<f2>") 'bm-remove-all-current-buffer)
(global-set-key (kbd "C-<f2>")   'bm-show-all)
(global-set-key (kbd "<f2>")     'bm-next)
(global-set-key (kbd "S-<f2>")   'bm-previous)
