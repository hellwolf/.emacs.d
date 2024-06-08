(require 'nix-mode nil t)
(require 'my-lib)

(use-package nix-mode
  :custom
  (fill-column 100)
  :hook ((nix-mode . (lambda () (add-hook 'before-save-hook #'nix-format-before-save)))))

(push (make-my-lang-mode
       :to-hook 'nix-mode-hook)
      my-prog-lang-modes)
