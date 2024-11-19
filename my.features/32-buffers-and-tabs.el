(require 'use-package)
(require 'tabbar nil t)

;; Reference:
;; - https://www.emacswiki.org/emacs/TabBarMode
;; - customize the look: https://gist.github.com/3demax/1264635

(use-package tabbar
  :custom
  (tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
  (tabbar-separator (quote (0.5)))

  :custom-face
  (tabbar-default
   ((t (:inherit variable-pitch :background "gray20" :foreground "gray20" :height 1.1
                 :box '(:line-width 5 :color "gray20" :style nil)))))

  (tabbar-unselected
   ((t (:inherit tabbar-default :background "gray30" :foreground "white"
                 :box '(:line-width 5 :color "gray30" :style released-button)))))
  (tabbar-modified
   ((t (:inherit tabbar-default :background "gray30" :foreground "green"
                 :box '(:line-width 5 :color "gray30" :style released-button)))))

  (tabbar-selected
   ((t (:inherit tabbar-default :background "gray85" :foreground "black" :height 1.15
                 :box '(:line-width 5 :color "gray85" :style pressed-button)))))
  (tabbar-selected-modified
   ((t (:inherit tabbar-default :background "gray85" :foreground "red" :height 1.15
                 :box '(:line-width 5 :color "gray85" :style pressed-button)))))

  (tabbar-highlight
   ((t (:inherit tabbar-default :background "white" :foreground "black" :underline t
                 :box '(:line-width 5 :color "white" :style nil)))))

  (tabbar-button
   ((t (:inherit tabbar-default
                 :box '(:line-width 1 :color "gray20" :style released-button)))))

  (tabbar-separator ((t (:height 0.6 :background "gray20"))))

  :hook
  ((after-save . my-tabbar-modification-state-change)
   (first-change . my-tabbar-on-buffer-modification))

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
  (set-buffer-modified-p t)
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
                  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open
buffers."
                  (interactive)
                  (switch-to-buffer (other-buffer (current-buffer) 1))))
;; Buffers-menu
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)
