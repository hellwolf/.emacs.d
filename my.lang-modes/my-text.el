(require 'my-lib)

(push (make-my-lang-mode
       :to-hook 'text-mode-hook)
      my-text-lang-modes)
