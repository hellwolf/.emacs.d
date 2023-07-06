(require 'use-package)
(require 'my-lib)

(use-package solidity-mode)
(push (make-my-lang-mode :to-hook 'solidity-mode-hook) my-prog-lang-modes)

(use-package yul-mode)
(push (make-my-lang-mode :to-hook 'yul-mode-hook) my-prog-lang-modes)
