(require 'nix-mode nil t)
(require 'my-lib)

(use-package nix-mode
  :pin manual
  :hook ((nix-mode . (lambda ()
                       (add-hook 'before-save-hook #'nix-format-before-save)
                       (setq fill-column 100)
                       ))))

(push (make-my-lang-mode
       :to-hook 'nix-mode-hook)
      my-prog-lang-modes)
