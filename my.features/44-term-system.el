(require 'term)
(require 'use-package)
(require 'vterm nil t)

(use-package vterm
  :custom
  (vterm-buffer-name-string "*term %s"))

(use-package vterm-toggle
  :config
  (define-key vterm-mode-map (kbd "C-S-t") 'vterm)
  (define-key vterm-mode-map (kbd "C-<prior>") 'vterm-toggle-backward)
  (define-key vterm-mode-map (kbd "C-<next>")  'vterm-toggle-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (C-`)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-`") 'vterm-toggle-cd)
(global-set-key (kbd "C-~") 'vterm-other-window)
