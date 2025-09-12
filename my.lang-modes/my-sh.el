;;; -*- lexical-binding: t -*-
(require 'my-lib)

(push (make-my-lang-mode
       :to-hook        'sh-mode-hook
       :org-babel-lang 'shell)
      my-prog-lang-modes)
