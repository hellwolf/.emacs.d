(require 'evil-ex nil t)
(require 'my-lib)

;; solidity-mode

(defun solidity-load-buffer ()
  (interactive)
  (save-buffer (current-buffer))
  (let ((evil-ex-original-buffer (current-buffer))) (evil-ex-execute "!solc --via-ir --ir %")))
;; with newer version: (evil-ex-execute "!solc --via-ir --ir %"))

(use-package solidity-mode
  :bind (:map
         solidity-mode-map ("C-c C-l" . #'solidity-load-buffer)))

(push (make-my-lang-mode
       :to-hook 'solidity-mode-hook)
      my-prog-lang-modes)

;; yul-mode

(defun yul-load-buffer ()
  (interactive)
  (save-buffer (current-buffer))
  (let ((evil-ex-original-buffer (current-buffer))) (evil-ex-execute "!solc --strict-assembly %")))
;; with newer version:  (evil-ex-execute "!solc --strict-assembly %"))

(use-package yul-mode
  :bind (:map
         yul-mode-map ("C-c C-l" . #'yul-load-buffer)))

(push (make-my-lang-mode
       :to-hook 'yul-mode-hook)
      my-prog-lang-modes)
