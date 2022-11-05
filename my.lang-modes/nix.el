(require 'use-package)
(require 'nix-mode nil t)
(require 'my-lib)

(use-package nix-mode)

(push (make-my-lang-mode
       :to-hook        'nix-mode-hook)
      my-prog-lang-modes)
