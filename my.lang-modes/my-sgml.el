;;; -*- lexical-binding: t -*-
(require 'my-lib)

(push (make-my-lang-mode
       :to-hook 'html-mode-hook)
      my-prog-lang-modes)
