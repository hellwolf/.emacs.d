(use-package editorconfig
  :pin manual
  :init (editorconfig-mode 1))
(use-package elisp-lint
  :pin manual)
(setq-default elisp-lint-ignored-validators '("indent" "checkdoc" "package-lint"))
