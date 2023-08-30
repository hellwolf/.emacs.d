(require 'use-package)
(require 'my-lib)

(use-package gnuplot)

(push (make-my-lang-mode
        :to-hook        'gnuplot-mode-hook
        :org-babel-lang 'gnuplot)
  my-prog-lang-modes)
