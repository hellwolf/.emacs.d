(require 'my-lib)

(defvar agda2-directory) ;; to be redefined in "agda2.el"

;; load agda-mode
(let ((agda2-el (shell-command-to-string "which agda-mode &>/dev/null && agda-mode locate")))
  (if (not (string-empty-p agda2-el)) (load-file agda2-el)))

(when agda2-directory
  (use-package agda2-mode :load-path agda2-directory ;; Need to specify the directory manually
    :mode (;; For .lagda.md files used in the PLFA book.
           ("\\.lagda.md\\'" . agda2-mode))
    :bind (:map
           agda2-mode-map
           ("C-c C-/" . #'agda2-goto-definition-keyboard)))
  (push (make-my-lang-mode
         :to-hook 'agda2-mode-hook)
        my-prog-lang-modes))
