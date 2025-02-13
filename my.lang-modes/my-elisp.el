(require 'lisp-mode)
(require 'my-lib)
(require 'flycheck)

(defun my-emacs-lisp-mode-hook ()
  ;; TODO not sure if these are still useful
  ;;(modify-syntax-entry ?- "w")
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

;; Include this package reference for build process.
(use-package elisp-lint
  :pin manual)

(push (make-my-lang-mode
       :to-hook        'emacs-lisp-mode-hook
       :my-hook        #'my-emacs-lisp-mode-hook
       :org-babel-lang 'emacs-lisp)
      my-prog-lang-modes)
