;;; -*- lexical-binding: t -*-
(require 'my-lib)

;; backup policies
(setq make-backup-files t)
(setq version-control t)
(setq delete-old-versions t)
(setq backup-directory-alist
  `(("." . ,(my-catfile user-emacs-directory "backup"))))

;; auto-save
(setq auto-save-list-file-prefix
  (my-catfile user-emacs-directory "backup" "auto-saves-"))
; https://emacs.stackexchange.com/questions/7729/autosave-scratch-to-a-directory
(setq-default auto-save-default t)
