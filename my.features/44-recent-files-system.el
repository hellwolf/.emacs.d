(require 'use-package)

(use-package recentf
  :custom
  (recentf-save-file (my-catfile user-emacs-directory ".cache" "recentf"))
  :init
  (recentf-mode t))
