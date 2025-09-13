;;; -*- lexical-binding: t -*-
(require 'use-package)
(require 'tabbar nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; blink-cursor-mode
(blink-cursor-mode t)

;; color highlighting
(global-font-lock-mode t)

;; set paren mode
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; highlight-symbol.el
(use-package highlight-symbol
  :pin manual)

;; reload file on changes
;; https://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)

;; truncate lines automatically
(set-default 'truncate-lines t)
(setq-default show-trailing-whitespace t)

;; display full column indicator (emacs 27+)
(if (fboundp 'global-display-fill-column-indicator-mode)
  (global-display-fill-column-indicator-mode))

;; mark tweaks
(transient-mark-mode t)
(setq mark-even-if-inactive t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reference:
;; - https://www.emacswiki.org/emacs/TabBarMode
;; - customize the look: https://gist.github.com/3demax/1264635

(use-package tabbar
  :pin manual
  :custom
  (tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
  (tabbar-separator (quote (0.5)))

  :custom-face
  (tabbar-default
   ((t (:inherit variable-pitch :background "gray20" :foreground "gray20" :height 1.1))))

  (tabbar-unselected
   ((t (:inherit tabbar-default :background "gray30" :foreground "white"))))
  (tabbar-modified
   ((t (:inherit tabbar-default :background "gray30" :foreground "green"))))

  (tabbar-selected
   ((t (:inherit tabbar-default :background "gray85" :foreground "black" :height 1.15))))
  (tabbar-selected-modified
   ((t (:inherit tabbar-default :background "gray85" :foreground "red" :height 1.15))))

  (tabbar-highlight
   ((t (:inherit tabbar-default :background "white" :foreground "black" :underline t))))

  (tabbar-button
   ((t (:inherit tabbar-default))))

  (tabbar-separator ((t (:height 0.6 :background "gray20"))))

  :hook
  ((after-save . #'my-tabbar-modification-state-change)
   (first-change . #'my-tabbar-on-buffer-modification)
  )

  :init
  (tabbar-mode))

(defun tabbar-close-tab-callback (event)
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event)))
           (b (current-buffer)))
      (kill-buffer (tabbar-tab-value (get-text-property (cdr target) 'tabbar-tab (car target))))
      (switch-to-buffer b))))

;; Called each time the modification state of the buffer changed.
(defun my-tabbar-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun my-tabbar-on-buffer-modification ()
  (my-tabbar-modification-state-change))

(defun my-tabbar-buffer-groups ()
  (list (cond
         ((string-prefix-p "*term " (buffer-name))                "terms")
         ((string-prefix-p "*scratch" (buffer-name))              "scratches")
         ((string-match    "[@*]irc\..*[*]?$" (buffer-name))      "irc")
         ((string-match    "^magit.*: " (buffer-name))            "magit")
         ((string-prefix-p "TAGS<" (buffer-name))                 "tags")
         ((string-equal    "*" (substring (buffer-name) 0 1))     "info")
         (t                                                       "user"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (C-<tab>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tabbar
(global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "<header-line> C-<mouse-1>") 'tabbar-close-tab-callback)

;; buffers
(global-set-key (kbd "C-x C-<tab>")
  (lambda ()
    "Switch to previously open buffer.Repeated invocations toggle between the
two most recently open
buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

;; Buffers-menu
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)
