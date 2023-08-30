(use-package editorconfig :init (editorconfig-mode 1))
(use-package elisp-lint)
(setq-default elisp-lint-ignored-validators '("indent" "checkdoc" "package-lint"))
