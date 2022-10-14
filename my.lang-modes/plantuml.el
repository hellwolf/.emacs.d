(require 'use-package)
(require 'my-lib)

(use-package plantuml-mode)

(push (make-my-lang-mode
        :to-hook        'plantuml-mode-hook
        :org-babel-lang 'plantuml)
  my-prog-lang-modes)
