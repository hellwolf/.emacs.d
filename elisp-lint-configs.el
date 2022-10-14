(use-package editorconfig
  :init
  (editorconfig-mode 1))
(use-package package-lint)
(use-package dash)
(use-package elisp-lint)

(require 'elisp-lint)
(setq-default
  elisp-lint-ignored-validators '("checkdoc" "byte-compile" "package-lint"))
