(require 'lisp-mode)
(require 'my-lib)
(require 'flycheck)

(defun my-emacs-lisp-mode-hook ()
  ;; TODO not sure if these are still useful
  ;;(modify-syntax-entry ?- "w")
  )

;; Include this package reference for build process.
(use-package elisp-lint)

(push (make-my-lang-mode
       :to-hook        'emacs-lisp-mode-hook
       :my-hook        #'my-emacs-lisp-mode-hook
       :org-babel-lang 'emacs-lisp)
      my-prog-lang-modes)
