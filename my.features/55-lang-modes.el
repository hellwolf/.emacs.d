(require 'use-package)
(require 'evil nil t)
(require 'diminish nil t)
(require 'lsp-headerline nil t)
(require 'highlight-symbol nil t)
(require 'my-lib)

;;; Adding features to programming and text modes.
;;; Each language modes has an entry in my.lang-modes/.

;; Modular in-buffer completion framework for Emacs
(use-package company)

;; install lsp-mode
(use-package lsp-mode
  :config
  (add-hook 'lsp-configure-hook
    (lambda ()
      (lsp-headerline-breadcrumb-mode -1))))

;; make white space visible
(defface extra-whitespace-face
  '((t (:background "pale green")))
  "Used for tabs and such."
  :group 'my-lang-modes)
(defvar my-extra-keywords '(("\t" . 'extra-whitespace-face)))

;; load configurations for major modes
(my-load-el-directory (my-catfile orig-user-emacs-directory "my.lang-modes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for all language modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-lang-mode-hook ()
  "Common language mode hooks"
  ;; for extra highlighting
  (font-lock-add-keywords nil my-extra-keywords)
  ;; evil default to normal state
  (setq-local evil-default-state 'normal)
  (evil-normal-state))

(defun add-lang-mode-hooks (m)
  "Add common hooks for language mode M."
  (add-hook m #'goto-address-mode)
  ;; show & highlight line number
  (add-hook m #'display-line-numbers-mode)
  ;; additional hooks
  (add-hook m #'my-lang-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for programming language modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-org-babel-languages () "List of org babel languages to be supported")

(defun my-prog-mode-hook ()
  ;; highlight tabs
  (font-lock-add-keywords nil my-extra-keywords)
  ;; no tabs
  (setq indent-tabs-mode nil)
  ;; underscore as word for snake cases
  (modify-syntax-entry ?_ "w")
  ;; support some greek letters too
  (mapc (lambda (l) (modify-syntax-entry l "w")) greek-letters)
  ;; diminishes
  (diminish #'eldoc-mode)
  (diminish #'hs-minor-mode)
  (diminish #'flyspell-mode)
  (diminish #'highlight-symbol-mode))

(mapc (lambda (x)
        (let ((m  (my-lang-mode-to-hook x))
               (h  (my-lang-mode-my-hook x))
               (ob (my-lang-mode-org-babel-lang x)))
          (add-lang-mode-hooks m)
          (add-hook m #'hs-minor-mode)
          (add-hook m #'flyspell-prog-mode)
          (add-hook m #'highlight-symbol-mode)
          (add-hook m #'which-function-mode)
          ;; [BUG] https://github.com/emacs-evil/evil/issues/721
          ;; (add-hook m #'superword-mode αVar test-αVar test_test_αVal )
          (add-hook m #'my-prog-mode-hook)
          (if h (add-hook m h))
          (if ob (push ob my-org-babel-languages))))
  my-prog-lang-modes)
(org-babel-do-load-languages 'org-babel-load-languages
  (mapcar (lambda (x) (cons x t)) my-org-babel-languages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for language modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-text-mode-hook ()
  ;; diminishes
  (diminish #'auto-fill-mode)
  (diminish #'flyspell-mode))

(mapc (lambda (x)
        (let ((m (my-lang-mode-to-hook x))
               (h (my-lang-mode-my-hook x)))
          (add-lang-mode-hooks m)
          (add-hook m #'turn-on-auto-fill)
          (add-hook m #'flyspell-mode)
          (add-hook m #'my-text-mode-hook)
          (if h (add-hook m h))))
  my-text-lang-modes)

;; set default major mode
(setq-default major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (F5-F9)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'compile)
;; (global-set-key (kbd "S-<f5>") 'ecb-toggle-compile-window)
;; (global-set-key (kbd "<f6>") 'emacsprj-sync)
;; (global-set-key (kbd "C-x <f6>") 'emacsprj-load)
;; (global-set-key (kbd "<f7>") 'add-change-log-entry-other-window)
