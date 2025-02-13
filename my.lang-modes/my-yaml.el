(require 'my-lib)

(use-package yaml-mode
  :pin manual)

(push (make-my-lang-mode
       :to-hook 'yaml-mode-hook)
      my-text-lang-modes)
