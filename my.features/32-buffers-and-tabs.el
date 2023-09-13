(require 'use-package)
(require 'tabbar nil t)

(use-package tabbar
  :config
  (defun my-tabbar-buffer-groups ()
    (list (cond
            ((string-prefix-p "*term " (buffer-name))         "terms")
            ((string-prefix-p "*scratch" (buffer-name))       "scratches")
            ((string-match "[@*]irc\..*[*]?$" (buffer-name))  "irc")
            ((string-prefix-p "magit: " (buffer-name))        "magit")
            ((string-prefix-p "TAGS<" (buffer-name))          "tags")
            ((string-equal "*" (substring (buffer-name) 0 1)) "info")
            (t                                                "user"))))
  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
  :init
  (tabbar-mode))

;; to be tested when emacs 27
;; (global-tab-line-mode)
;; (tab-bar-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (C-<tab>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer & tabbar
(global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x C-<tab>")
  (lambda ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1))))
