(require 'use-package)
(require 'my-lib)

(use-package yaml-mode)

(push (make-my-lang-mode :to-hook 'yaml-mode-hook) my-text-lang-modes)
