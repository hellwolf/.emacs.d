(require 'my-lib)

(use-package python
  :pin manual)

(push (make-my-lang-mode
       :to-hook        'python-mode-hook
       :org-babel-lang 'python)
      my-prog-lang-modes)
