(require 'sgml-mode)
(require 'my-lib)

(setq sgml-basic-offset 4)

(push (make-my-lang-mode :to-hook 'html-mode-hook) my-prog-lang-modes)
