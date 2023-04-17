(require 'lisp-mode)
(require 'my-lib)

(defun my-emacs-lisp-mode-hook ()
  (modify-syntax-entry ?- "w"))

(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

(push (make-my-lang-mode
        :to-hook        'emacs-lisp-mode-hook
        :my-hook        #'my-emacs-lisp-mode-hook
        :org-babel-lang 'emacs-lisp)
  my-prog-lang-modes)
