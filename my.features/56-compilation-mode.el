;;; -*- lexical-binding: t -*-
(require 'compile)

(add-hook 'compilation-mode-hook
  (lambda ()
    (setq truncate-lines nil)
    (setq truncate-partial-width-windows nil)))

;; scroll scroll
(setq compilation-scroll-output t)
