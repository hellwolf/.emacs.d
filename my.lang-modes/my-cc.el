;;; -*- lexical-binding: t -*-
(require 'cc-mode)
(require 'my-lib)

(defun my-cc-mode-hook ()
  (setq c-basic-offset 4)
  (setq c-default-style "stroustrup")
  ;; auto hungry mode
  ;; (c-toggle-auto-hungry-state t)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'innamespace 0))

(push (make-my-lang-mode
        :to-hook 'c-mode-hook
        :my-hook #'my-cc-mode-hook
        :org-babel-lang 'C)
      my-prog-lang-modes)

(push (make-my-lang-mode
        :to-hook 'c++-mode-hook
        :my-hook #'my-cc-mode-hook
        :org-babel-lang 'C)
      my-prog-lang-modes)
