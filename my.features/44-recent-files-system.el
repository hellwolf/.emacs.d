;;; -*- lexical-binding: t -*-
(require 'use-package)
(require 'my-lib)
(require 'recentf)

(setq recentf-save-file (my-catfile user-emacs-directory ".cache" "recentf"))
(recentf-mode t)
