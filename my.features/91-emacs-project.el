(require 'use-package)
(require 'editorconfig nil t)
(require 'my-lib)
(require 'desktop)

;; editorconfig
(use-package editorconfig :diminish)

(defun open-emacs-project (pdir)
  "Open a project configured with .emacs-project"
  (interactive "DDirectory of a .emacs-project: ")
  (let*
    ((data-dir (my-catfile pdir ".emacs-project"))
      (project-el-file (my-catfile pdir ".emacs-project.el"))
      (project-el-file2 (my-catfile pdir "emacs-project.el")))
    (message (concat "Opening emacs-project: " emacs-project-root-directory))
    ;; use editorconfig
    (editorconfig-mode 1)
    ;; desktop session integration
    (desktop-read data-dir)
    ;; load .emacs-project.el
    (if (file-readable-p project-el-file) (load project-el-file))
    (if (file-readable-p project-el-file2) (load project-el-file2))
    (add-hook 'kill-emacs-hook
      `(lambda () (ignore-errors (desktop-save ,data-dir t))))))
