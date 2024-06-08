(require 'haskell nil t)
(require 'haskell-doc nil t)
(require 'lsp-mode nil t)
(require 'flycheck nil t)
(require 'my-lib)

(use-package haskell-mode
  :pin manual
  :custom
  (haskell-stylish-on-save t)
  (haskell-font-lock-symbols t)
  ;; https://github.com/haskell/haskell-language-server/discussions/3105
  (lsp-lens-place-position 'above-line)
  ;; (haskell-font-lock-symbols t)
  :hook ((haskell-mode . #'haskell-mode-init)
         (haskell-literate-mode . #'haskell-mode-init))
  :bind (:map
         haskell-mode-map
         ;; https://emacs.stackexchange.com/questions/59254/how-to-bind-key-in-use-package/59269#59269
         ("C-x C-r" . #'my-haskell-load-and-run)
         :map
         interactive-haskell-mode-map
         ("M-." . #'xref-find-definitions)))

(use-package lsp-haskell)

(defun haskell-mode-init()
  (interactive-haskell-mode)
  (haskell-doc-mode)
  (lsp)
  (xref-etags-mode))

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

;; Disabling stack-ghc flychecker when I am using stack, it is very slow otherwise.
;; See: https://emacs.stackexchange.com/questions/53667/how-to-disable-stack-with-flycheck-for-haskell
(add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)

;; Display lambda symbol instead of backslash.
(custom-set-variables
 '(haskell-font-lock-symbols-alist '(("\\" . "λ"))))
