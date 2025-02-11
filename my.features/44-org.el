(require 'use-package)
(require 'evil-org-mode nil t)
(require 'my-lib)

(use-package org
  :hook
  (org-mode . org-indent-mode)
  :custom
  (org-support-shift-select 'always)
  (org-adapt-indentation nil)
  ;; (org-hide-emphasis-markers t)
  ;; better inline image preview
  (org-image-actual-width nil)
  ;; org-todo
  (org-todo-keywords '((sequence "TODO" "WIP" "AWAIT" "|" "DONE" "DELEGATED")))
  ;; org-babel
  (org-confirm-babel-evaluate nil)
  ;; https://stackoverflow.com/questions/53469017/org-mode-source-editing-indents-code-after-exiting-source-code-block-editor
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  :config
  ;; org-tempo doesn't work for emacs 26 (ubuntu 20)
  (add-to-list 'org-modules 'org-tempo t)
  ;; this is a hack for ownCloud, since it can't render .org files
  ;; but it is also good for general literate programming to denote intended programming language
  (add-to-list 'auto-mode-alist '("\\.org.[^.]*\\'" . org-mode)))

;; Beautify Org
;; https://github.com/zzamboni/dot-emacs/blob/master/init.org#beautifying-org-mode
(use-package org-bullets
  :after (org)
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge)
  :hook
  (org-mode . (lambda ()
                (message "org-mode-hook for org-bullets")
                ;; Display the titles with nice unicode bullets instead of the text ones
                (org-bullets-mode 1)
                ;; Beautify Org Checkbox Symbol
                (push '("[ ]" . "⬛" ) prettify-symbols-alist)
                (push '("[x]" . "❎" ) prettify-symbols-alist)
                (push '("[X]" . "✅" ) prettify-symbols-alist)
                (push '("[-]" . "🔲" ) prettify-symbols-alist)
                (push '("[=]" . "⬜" ) prettify-symbols-alist)
                (push '("[?]" . "❓" ) prettify-symbols-alist)
                (prettify-symbols-mode 1)
                ;; Headers
                (let* (
                        (mono-font
                          (cond
                            ((x-list-fonts   "Fira Mono") '(:font   "Fira Mono"))
                            ((x-family-fonts "Monospace") '(:family "Monospace"))
                            (nil (warn "Cannot find a mono font."))))
                        (sans-font
                          (cond
                            ((x-list-fonts   "Fira Sans")  '(:font   "Fira Sans"))
                            ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                            (nil (warn "Cannot find a sans serif font."))))
                        (base-font-color (face-foreground 'default nil 'default))
                        (levelline `(:inherit default :foreground ,base-font-color))
                        (headline  `(:inherit default :weight bold :foreground ,base-font-color))
                        (titleline `(:inherit default :weight bold :underline t :foreground "LawnGreen"))
                        (docinfo   `(:inherit default :foreground "ForestGreen"))
                        (metaline  `(:inherit default :foreground "SkyBlue")))
                  (custom-theme-set-faces
                    'user
                    `(org-level-8               ((t (,@levelline ,@sans-font))))
                    `(org-level-7               ((t (,@levelline ,@sans-font))))
                    `(org-level-6               ((t (,@levelline ,@sans-font))))
                    `(org-level-5               ((t (,@levelline ,@sans-font))))
                    `(org-level-4               ((t (,@levelline ,@sans-font :height 1.00))))
                    `(org-level-3               ((t (,@levelline ,@sans-font :height 1.10))))
                    `(org-level-2               ((t (,@levelline ,@sans-font :height 1.20))))
                    `(org-level-1               ((t (,@levelline ,@sans-font :height 1.40))))
                    `(org-document-title        ((t (,@titleline ,@sans-font :height 1.50))))
                    `(org-document-info         ((t (,@docinfo   ,@mono-font :height 1.30))))
                    `(org-document-info-keyword ((t (,@docinfo   ,@mono-font :height 1.30))))
                    `(org-meta-line             ((t (,@metaline  ,@mono-font :height 1.20))))
                    )))))

;; evil-org
(use-package evil-org
  :after (org)
  :hook (org-mode . evil-org-mode))

;; epresent.el
;; NOTE:
;; - reference: https://github.com/eschulte/epresent/blob/master/present.org?plain=1
;; - sub-level support: EPRESENT_FRAME_LEVEL
(use-package epresent)

(push (make-my-lang-mode :to-hook 'org-mode-hook) my-text-lang-modes)
