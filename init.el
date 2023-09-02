;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preemble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; additional load-path
(add-to-ordered-list 'load-path (concat user-emacs-directory "libs"))
(add-to-ordered-list 'load-path (concat user-emacs-directory "3rd-party-libs"))

(require 'my-lib)

;; setup directories
(setq orig-user-emacs-directory user-emacs-directory)
(setq my-tools-path (my-catfile user-emacs-directory "tools"))
(defvar my-private-pre-el-path (my-catfile user-emacs-directory "my.private.pre"))
(defvar my-features-path (my-catfile user-emacs-directory "my.features"))
(defvar my-private-post-el-path (my-catfile user-emacs-directory "my.private.post"))

;; other settings
(setq debug-on-error init-file-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example: https://github.com/tjaartvdwalt/emacs-config/blob/master/elpa.el

(require 'package)

;; Selected package archives
(setq package-archives
      '(('ELPA-GNU     . "https://elpa.gnu.org/packages/")
        ('ELPA-NONGNU  . "https://elpa.nongnu.org/nongnu/")
        ('MELPA        . "https://melpa.org/packages/")
        ('MELPA-STABLE . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(('MELPA        . 10)
        ('ELPA-GNU     .  5)
        ('ELPA-NONGNU  .  3)
        ('MELPA-Stable .  0)))
(package-initialize)

;; Its neccesary to update your repos on first install
;; otherwise elpa cannot find the sources. Once the archives
;; dir exists, you will have to manually update
(unless (file-exists-p "~/.emacs.d/elpa/archives")
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; https://jwiegley.github.io/use-package/keywords/
(require 'use-package-ensure)
(setq
  use-package-always-ensure t
  use-package-compute-statistics t
  use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (my-catfile my-private-post-el-path "99-default-custom.el"))

;; save the default current settings
(defun load-hw-emacs ()
  (progn
    ;; load all
    (my-load-el-directory my-private-pre-el-path)
    (my-load-el-directory my-features-path)
    (my-load-el-directory my-private-post-el-path)
    ;; save default-custom.el
    (let
      (delete-file c)
      (ignore-errors (customize-save-customized)))))

;; Command line option "-P" to open an `.emacs-project`
(add-to-list
  'command-switch-alist
  '("-P" . (lambda (switch)
             (let* ((pdir (expand-file-name (pop command-line-args-left)))
                     (udir (my-catfile pdir ".emacs-project")))
               (if (file-exists-p pdir)
                 (progn
                   (message (concat "emacs-project directory: " pdir ":" udir))
                   (setq emacs-project-root-directory pdir)
                   (setq user-emacs-directory udir)
                   (unless (file-directory-p udir) (make-directory udir))
                   (load-hw-emacs)
                   (open-emacs-project emacs-project-root-directory))
                 (warn (concat "Not a directory: " pdir)))))))

;; Command line option "--load-hw-emacs"
(add-to-list
  'command-switch-alist
  '("--load-hw-emacs" . (lambda (switch) (load-hw-emacs))))
