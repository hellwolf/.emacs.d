(require 'my-lib)

;; load agda-mode
(let ((a (shell-command-to-string "which agda-mode &>/dev/null && agda-mode locate")))
  (if (not (string-empty-p a)) (load-file (let ((coding-system-for-read 'utf-8)) a))))

;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
      (append
       '(("\\.agda\\'" . agda2-mode)
         ("\\.lagda.md\\'" . agda2-mode))
       auto-mode-alist))

(push (make-my-lang-mode
       :to-hook 'agda2-mode-hook)
      my-prog-lang-modes)
