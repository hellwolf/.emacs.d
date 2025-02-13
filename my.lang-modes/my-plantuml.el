(require 'my-lib)

(use-package plantuml-mode
  :pin manual
  :custom
  ;; search plantuml jar path in the nix environment.
  (plantuml-jar-path (shell-command-to-string
                      "trace-which plantuml | tail -n1 | sed 's:bin/plantuml:lib/plantuml.jar:' | tr -d '\\n'")))

(push (make-my-lang-mode
       :to-hook        'plantuml-mode-hook
       :org-babel-lang 'plantuml)
      my-prog-lang-modes)
