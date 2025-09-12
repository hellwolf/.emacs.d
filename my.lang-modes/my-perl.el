;;; -*- lexical-binding: t -*-
(require 'cperl-mode)
(require 'my-lib)

(use-package cperl-mode ;; built-in
  :mode
  (;; perl xs file
   ("\\.xs$" . c-mode)
   ;; perl mode for psh
   ("\\.psh$" . perl-mode))
  :custom
  ;; indention adjustment
  (cperl-indent-parens-as-block t)
  (cperl-tab-always-indent t))

(push (make-my-lang-mode
       :to-hook        'cperl-mode-hook
       :org-babel-lang 'perl)
      my-prog-lang-modes)
