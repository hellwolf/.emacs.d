;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'my-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Search Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @dev search variable by ~xfunc~
(defun my-search-variables-by-x (r xfunc byname)
  (with-output-to-temp-buffer "*Help*"
    (with-current-buffer (get-buffer "*Help*")
      (princ "Variables matches ")
      (my-princ-with-properties (format "%s :\n\n" r)
        '(face bold))
      (mapatoms
        (lambda (s)
          (if (and (or byname (boundp s))
                (stringp (funcall xfunc s))
                (string-match r (funcall xfunc s)))
            (progn
              (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1]
                  (lambda (event)
                    (interactive "e")
                    (save-excursion
                      (mouse-set-point event)
                      (describe-variable
                        (intern (get-text-property (point) 'symname))))))
                (my-princ-with-properties (symbol-name s)
                  (list
                    'symname (symbol-name s)
                    'face 'button
                    'mouse-face 'highlight
                    'keymap map)))
              (princ "\n")
              (or byname (princ (concat (symbol-value s) "\n\n")))
              ))))
      (help-print-return-message)))
  (help-setup-xref (list #'my-search-variables-by-x r)
    (called-interactively-p 'interactive)))

;; @dev search variables by value
(defun my-search-variables-by-value (r)
  (interactive "MSearch variable whose values match this regex:")
  (my-search-variables-by-x r 'symbol-value nil))

;; @dev search variables by name
(defun my-search-variables-by-name (r)
  (interactive "MSearch variable whose names match this regex:")
  (my-search-variables-by-x r 'symbol-name t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show & copy buffer name
(defun my-show-and-copy-buffer-name ()
  (interactive)
  (message buffer-file-name)
  (kill-new buffer-file-name))

;; show & copy file to kill-ring
(defun my-copy-file-to-kill-ring (&optional _)
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scratch Buffer Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; switch to scratch and back
(defun switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

        COMMAND -> Toggle the `*scratch*' buffer
    C-0 COMMAND -> Toggle a scratch buffer in `fundamental-mode'
    C-- COMMAND -> Toggle a scratch buffer in the current buffer's major mode
    C-u COMMAND -> Toggle a scratch buffer in `org-mode'
C-u C-u COMMAND -> Toggle a scratch buffer in `emacs-elisp-mode'

Return the scratch buffer opened.

Original: https://emacs.stackexchange.com/questions/80/
how-can-i-quickly-toggle-between-a-file-and-a-scratch-buffer-having-the-same-m
"
  (interactive "pSwitch to a scratch buffer")
  (if (and (or (null arg)               ; no prefix
             (= arg 1))
        (string-match-p "\\*scratch" (buffer-name)))
    (delete-window) ; vs. (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0  "fundamental-mode") ; C-0
                       (-1 (format "%s" major-mode)) ; C-
                       (4  "org-mode") ; C-u
                       (16 "emacs-lisp-mode") ; C-u C-u
                       (t  nil)))
            (buf (get-buffer-create (concat "*scratch" (if mode-str "-") mode-str "*"))))
      (pop-to-buffer buf) ; vs. (switch-to-buffer buf)
      ;; switch mode: http://stackoverflow.com/a/7539787/1219634
      (if (stringp mode-str) (funcall (intern mode-str)))
      buf)))

(defun scratch-in-same-mode  () (interactive) (switch-to-scratch-and-back -1))
(defun scratch-in-org-mode   () (interactive) (switch-to-scratch-and-back 4))
(defun scratch-in-elisp-mode () (interactive) (switch-to-scratch-and-back 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (C-c-, F12)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy utilities
(global-set-key (kbd "C-c C-b") 'my-show-and-copy-buffer-name)
(global-set-key (kbd "C-c C-f") 'my-copy-file-to-kill-ring)
;; toggle scratch and info buffers
(global-set-key (kbd "<f12>")       'switch-to-scratch-and-back)
(global-set-key (kbd "S-<f12>")     'scratch-in-same-mode)
(global-set-key (kbd "C-x <f12> o") 'scratch-in-org-mode)
(global-set-key (kbd "C-x <f12> e") 'scratch-in-elisp-mode)
(global-set-key (kbd "C-x <f12> m") 'scratch-in-same-mode) ; TODO message buffer
(global-set-key (kbd "C-x <f12> w") 'scratch-in-same-mode) ; TODO info buffer
