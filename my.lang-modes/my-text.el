(require 'my-lib)

(use-package flycheck-grammarly
  :pin manual
  :init (flycheck-grammarly-setup))

(push (make-my-lang-mode
       :to-hook 'text-mode-hook)
      my-text-lang-modes)
