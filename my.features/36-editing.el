;;; -*- lexical-binding: t -*-
(require 'use-package)
(require 'diminish nil t)
(require 'evil-core nil t)
(require 'flycheck nil t)
(require 'undo-tree nil t)
(require 'my-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer editing configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tab configuration
(setq tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; https://stackoverflow.com/questions/13574564/emacs-automatically-adding-a-newline-even-after-changing-emacs
(setq mode-require-final-newline nil)

;; https://stackoverflow.com/questions/7746965/how-do-you-delete-trailng-white-space-in-emacs
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; autofill mode
(auto-fill-mode t)
(setq-default fill-column 120)
(diminish 'auto-fill-mode)

;; typed text replaces the selection
(delete-selection-mode t)

;; abbrev-mode on by default
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)

;; re-enable disabled commands related to regions
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :pin manual
  :custom
  ;; undo behaviour
  (evil-undo-system 'undo-tree)
  (evil-want-fine-undo 'yes)
  ;; evil search
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; stick to emacs bindings in evil insert mode
  (evil-disable-insert-state-bindings t)
  ;; evil cursors
  (evil-normal-state-cursor 'hollow)
  (evil-insert-state-cursor '(bar . 1))
  (evil-emacs-state-cursor 'box)
  ;; default to emacs mode except for prog/text modes
  (evil-default-state 'emacs)
  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (evil-symbol-word-search t)
  :config
  ;; Remap to emacs movement keybindings in normal mode
  ;; (define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
  ;; (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  ;; (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
  ;; (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  :init
  (evil-mode))

;; [BUG] Terminal Emacs: cursor shape unchanged under insert mode
;; https://github.com/doomemacs/doomemacs/issues/1994
(use-package evil-terminal-cursor-changer
  :pin manual
  :hook ((tty-setup . evil-terminal-cursor-changer-activate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spellchecks, linting, and error system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flycheck
(use-package flycheck
  :pin manual
  :diminish ""
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :init
  (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clipboard system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package browse-kill-ring
  :pin manual)

;; clipboard tweaks
; Non-nil means cutting and pasting uses the clipboard.
(setq select-enable-clipboard t)
; If non-nil, copy to kill ring upon mouse adjustments of the region.
(setq mouse-drag-copy-region nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; undo-tree
(use-package undo-tree
  :pin manual
  :diminish ""
  :custom
  (undo-tree-history-directory-alist `(("." . ,(my-catfile user-emacs-directory "undo-tree"))))
  :init
  (global-undo-tree-mode)
  (add-hook 'undo-tree-mode-hook
    (lambda () (define-key undo-tree-map (kbd "C-/") nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq case-fold-search nil) ; Non-nil if searches and matches should ignore case

;; wgrep
(use-package wgrep
  :pin manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Visible bookmarks in buffer for GNU Emacs 26.x, 27.x
;; https://github.com/joodland/bm
(use-package bm
  :pin manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; input-method switching
;; https://stackoverflow.com/questions/10192341/how-to-enter-greek-characters-in-emacs
(defun my-switch-to-greek ()
  (interactive)
  (set-input-method 'greek))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump between marks
(global-set-key (kbd "C-<") #'my-point-to-register)
(global-set-key (kbd "C->") #'my-jump-to-register)
;; jump to special marks
(global-set-key (kbd "C-S-g") #'goto-line)
(global-set-key (kbd "C-c a") #'my-go-to-char)
;; rectangle marks
(global-set-key (kbd "C-S-SPC") #'cua-rectangle-mark-mode)

;; bookmark navigation (<F2>), similar to https://atom.io/packages/bookmarks
(global-set-key (kbd "C-S-<f2>") 'bm-toggle)
(global-set-key (kbd "M-S-<f2>") 'bm-remove-all-current-buffer)
(global-set-key (kbd "C-<f2>")   'bm-show-all)
(global-set-key (kbd "<f2>")     'bm-next)
(global-set-key (kbd "S-<f2>")   'bm-previous)

;; error navigation (<F3>)
(global-set-key (kbd "<f3>") #'next-error)
(global-set-key (kbd "S-<f3>") #'previous-error)

;; input methods (C-x \\, C-x C-\\)
(global-set-key (kbd "C-x C-\\ g") #'my-switch-to-greek)

;; misc
(global-set-key (kbd "C-/") #'comment-line)
(global-set-key (kbd "C-c SPC") #'just-one-space)
