;;; -*- lexical-binding: t -*-
(require 'use-package)
(require 'which-key-posframe nil t)
(require 'ido)

;; using built-in ido
;; alternatives:
;; - https://emacs-helm.github.io/helm/
;; - https://oremacs.com/swiper/
(setq ido-case-fold t) ; Non-nil if searching of buffer and file names should ignore case.
(setq ido-everywhere t) ; Non-nil if Ido-Everywhere mode is enabled.
(ido-mode t)

;; https://github.com/justbur/emacs-which-key
;; Perhaps one of the most useful extensions, this little gem will provide a list in the mini-buffer of the relevant
;; keystrokes and the functions to which they are bound (or a prefix). Many times I've found unknown features by simply
;; looking at the various options. This is, IMO, a great way to learn Emacs key-bindings.
(use-package which-key
  :pin manual
  :diminish ""
  :init
  (which-key-mode t))
(use-package which-key-posframe
  :pin manual
  :init
  (which-key-posframe-mode t)
  :custom
  (set-face-attribute 'which-key-posframe nil :inherit default :background nil)
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner))
