(require 'python)
(require 'my-lib)

(push (make-my-lang-mode
        :to-hook        'python-mode-hook
        :org-babel-lang 'python)
  my-prog-lang-modes)
