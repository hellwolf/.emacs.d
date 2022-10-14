(require 'use-package)
(require 'haskell nil t)
(require 'my-lib)

(use-package haskell-mode
  :custom
  (haskell-stylish-on-save t)
  ;; https://github.com/haskell/haskell-language-server/discussions/3105
  (lsp-lens-place-position 'above-line)
  ;; (haskell-font-lock-symbols t)
  :hook ((haskell-mode . interactive-haskell-mode)
          (haskell-mode . haskell-doc-mode)
          (haskell-mode . lsp)
          (haskell-literate-mode . lsp))
  :bind (;; https://emacs.stackexchange.com/questions/59254/how-to-bind-key-in-use-package/59269#59269
          :map haskell-mode-map
          ("C-x C-r" . #'my-haskell-load-and-run)))

(use-package lsp-haskell)

(defun my-haskell-load-and-run()
  "Load and run the current Haskell file main function."
  (interactive)
  (my-point-to-register)
  (let ((start-buffer (current-buffer)))
    (haskell-process-load-file)
    (haskell-interactive-bring)
    (haskell-interactive-mode-run-expr "main")
    (sleep-for 0 100)
    (goto-char (point-max)) ;; (end-of-buffer)
    (pop-to-buffer start-buffer)
    (my-jump-to-register)))

(push (make-my-lang-mode
        :to-hook        'haskell-mode-hook
        :org-babel-lang 'haskell)
  my-prog-lang-modes)
