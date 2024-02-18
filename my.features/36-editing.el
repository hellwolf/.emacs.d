(require 'use-package)
(require 'etags nil t)
(require 'my-lib)

;; tab width
(setq tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; https://stackoverflow.com/questions/13574564/emacs-automatically-adding-a-newline-even-after-changing-emacs
(setq mode-require-final-newline nil)

;; https://stackoverflow.com/questions/7746965/how-do-you-delete-trailng-white-space-in-emacs
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; autofill mode
(auto-fill-mode -1)
(setq-default fill-column 120)
(diminish 'auto-fill-mode)

;; typed text replaces the selection
(delete-selection-mode t)

;; down/up-case region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; flycheck
(use-package flycheck
  :diminish ""
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :init
  (global-flycheck-mode))

;; undo-tree
(use-package undo-tree
  :diminish ""
  :custom
  (undo-tree-history-directory-alist `(("." . ,(my-catfile user-emacs-directory "undo-tree"))))
  :init
  (global-undo-tree-mode)
  (add-hook 'undo-tree-mode-hook
    (lambda () (define-key undo-tree-map (kbd "C-/") nil))))

;; wgrep
(use-package wgrep)

;; abbrev-mode on by default
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)

;; search/replace configuration
(setq case-fold-search nil)

;; input-method switching
;; https://stackoverflow.com/questions/10192341/how-to-enter-greek-characters-in-emacs
(defun my-switch-to-greek()
  (interactive)
  (set-input-method 'greek))

;; TAGS
(setq tags-revert-without-query 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input methods
(global-set-key (kbd "C-x C-\\ g") #'my-switch-to-greek)
;; comments
(global-set-key (kbd "C-/") #'comment-line)
;; misc keys
(global-set-key (kbd "C-c SPC") #'just-one-space)
;; marks
(global-set-key (kbd "C-<") #'my-point-to-register)
(global-set-key (kbd "C->") #'my-jump-to-register)
;; rectangle marks
(global-set-key (kbd "C-S-SPC") #'cua-rectangle-mark-mode)
;; others
(global-set-key (kbd "C-S-g") #'goto-line)
(global-set-key (kbd "C-c a") #'my-go-to-char)
;; next/prev
(global-set-key (kbd "<f3>") 'next-error)
(global-set-key (kbd "S-<f3>") 'previous-error)
