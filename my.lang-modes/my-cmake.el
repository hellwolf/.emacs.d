;;; -*- lexical-binding: t -*-
(require 'my-lib)

(use-package cmake-mode
  :pin manual
  :mode "CMakeLists.txt")

(push (make-my-lang-mode
       :to-hook 'cmake-mode-hook)
      my-prog-lang-modes)
