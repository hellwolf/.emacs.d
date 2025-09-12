;;; -*- lexical-binding: t -*-
(require 'use-package)
(require 'htmlize nil t)
(require 'ox-html nil t)


;; https://emacs.stackexchange.com/questions/3374/set-the-background-of-org-exported-code-blocks-according-to-theme
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
(use-package htmlize
  :pin manual
  :config
  (setq htmlize-output-type 'inline-css)
  )

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-functions 'my/org-inline-css-hook)

;; htmlize buffer view immediately
(defun my-htmlize-buffer-view ()
  (interactive)
  (let ((new-name (concat "~/tmp/htmlize-"
                    (buffer-name (current-buffer))
                    ".html"))
         (new-buffer (htmlize-buffer)))
    (with-current-buffer new-buffer (write-file new-name))
    (kill-buffer new-buffer)
    (browse-url new-name)))

(defun my-htmlize-region-view (beg end)
  (interactive "r")
  (let ((new-name (concat "~/tmp/htmlize-"
                    (buffer-name (current-buffer))
                    ".snippet.html"))
         (new-buffer (htmlize-region beg end)))
    (with-current-buffer new-buffer (write-file new-name))
    (kill-buffer new-buffer)
    (browse-url new-name)))
