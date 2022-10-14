(require 'term)

;; popup a term window
(defvar my-term-number 0)
(defvar my-term-buffer-toggled nil)
(defun my-term-pop-to (buffer)
  (pop-to-buffer buffer
    '((display-buffer-reuse-window display-buffer-at-bottom))))
(defun my-term-create-new ()
  (interactive)
  (let ((term-name (concat "term-" (int-to-string my-term-number))))
    (setq my-term-number (1+ my-term-number))
    (make-term term-name "/bin/bash")
    (my-term-pop-to (get-buffer (concat "*" term-name "*")))
    (term-mode)
    (term-char-mode)
    (visual-line-mode t)))

(defun my-term-toggle ()
  (interactive)
  (if (string-match "^\\*term-[0-9]+\\*$"
        (buffer-name (current-buffer)))
    (progn
      (setq my-term-buffer-toggled (current-buffer))
      (delete-window))
    (let ((last-term-window (car (last (seq-filter
                                         (lambda (wnd) (string-prefix-p "*term-" (buffer-name (window-buffer wnd))))
                                         (window-list))))))
      ;; if there is a terminal currently open in a window
      (if last-term-window (select-window last-term-window)
        ;; else find a recently used term (one that's toggled recently, or the last from the buffer list)
        (let ((last-term-buffer (if (buffer-live-p my-term-buffer-toggled)
                                  my-term-buffer-toggled
                                  (car (last (seq-filter
                                               (lambda (buf) (string-prefix-p "*term-" (buffer-name buf)))
                                               (buffer-list)))))))
          ;; found the recently used term, switching to it
          (if last-term-buffer
            (my-term-pop-to last-term-buffer)
            ;; not found, creating new one
            (my-term-create-new)))))))

;; close term buffer window when finished
(advice-add 'term-handle-exit :after
  (lambda (process-name msg)
    (when (string-prefix-p "*term-" (buffer-name (current-buffer))) (kill-buffer-and-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (C-`)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-`") 'my-term-toggle)
(global-set-key (kbd "C-~") 'my-term-create-new)
