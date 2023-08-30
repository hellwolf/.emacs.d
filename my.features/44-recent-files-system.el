(require 'use-package)
(require 'recentf)

(setq recentf-save-file (my-catfile user-emacs-directory ".cache" "recentf"))
(recentf-mode t)
