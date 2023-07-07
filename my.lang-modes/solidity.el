(require 'use-package)
(require 'my-lib)
(require 'evil-ex)

(use-package solidity-mode)
(push (make-my-lang-mode :to-hook 'solidity-mode-hook) my-prog-lang-modes)

(defun yul-load-buffer ()
  (interactive)
  (save-buffer (current-buffer))
  (let ((evil-ex-current-buffer (current-buffer))) (evil-ex-execute "!solc --strict-assembly %")))

(use-package yul-mode
  :bind (:map yul-mode-map
              ("C-c C-l" . #'yul-load-buffer)))
(push (make-my-lang-mode :to-hook 'yul-mode-hook) my-prog-lang-modes)
