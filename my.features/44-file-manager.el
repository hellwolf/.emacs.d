;;; -*- lexical-binding: t -*-
(require 'use-package)
(require 'my-lib)
(require 'treemacs nil t)
(require 'treemacs-evil nil t)
(require 'fzf nil t)
(require 'etags nil t)
(require 'dired-x) ; a bit more retro flavor

;; treemacs
;;

(use-package treemacs
  :pin manual)

;; treemacs-evil
(use-package treemacs-evil
  :pin manual
  :custom (treemacs-follow-mode nil)
  :after (treemacs evil))

;; fzf
;;

;; https://github.com/bling/fzf.el
;; An Emacs front-end for fzf.
(use-package fzf
  :pin manual)

(defun my-fzf-find-file ()
  (interactive)
  (if (vc-root-dir)
      (fzf-with-command "git ls-files" #'fzf--action-find-file (vc-root-dir))
    (fzf-find-file emacs-project-root-directory)))

;; etags
;;

(setq tags-revert-without-query 1) ; Non-nil means reread a TAGS table without querying, if it has changed.

;; vc
;;

(defun my-vc-grep-file ()
  (interactive)
  (if (vc-root-dir)
      (call-interactively 'vc-git-grep)
    (let ((default-directory emacs-project-root-directory)) (call-interactively 'rgrep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file search (F4)
(global-set-key (kbd "<f4>") #'my-fzf-find-file)
(global-set-key (kbd "C-<f4>") #'my-vc-grep-file)

;; treemacs (F9)
(global-set-key (kbd "C-<f9>") #'treemacs)
(global-set-key (kbd "C-x C-<f9> d") #'dired)
(global-set-key (kbd "C-x 4 C-<f9> d") #'dired-other-window)
