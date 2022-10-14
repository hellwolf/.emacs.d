(require 'gas-mode)
(require 'my-lib)

(setq auto-mode-alist
  (append '(("\\.S$" . gas-mode))
    auto-mode-alist))

(push (make-my-lang-mode :to-hook 'gas-mode-hook) my-prog-lang-modes)
