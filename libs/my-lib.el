;;; -*- lexical-binding: t -*-
(provide 'my-lib)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my directories
(defvar my-tools-path nil "My tools directory")

;; emacs-project support
(defvar emacs-project-root-directory nil "The emacs-project root folder")
(defvar orig-user-emacs-directory nil "Original user emacs directory")

;; lang-modes
(cl-defstruct my-lang-mode to-hook my-hook org-babel-lang)
(defvar my-prog-lang-modes () "List of programming language modes")
(defvar my-text-lang-modes () "List of text language modes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @dev mimic of perl's catfile`
(defun my-catfile (&rest entries)
  (let ((fst (car entries))
         (rest (cdr entries)))
    ;; preserve the absolute/relative dir for the first entry
    (concat (file-name-as-directory fst)
      (mapconcat 'identity
        (seq-filter (lambda (e) (not (string-empty-p e)))
          (apply #'append
            (mapcar (lambda (e) (split-string e "/")) rest)))
        "/"))))

;; @dev load scripts from an el directory
(defun my-load-el-directory (path)
  (when (file-accessible-directory-p path)
    (mapc 'load (directory-files path t "[^.].+\\.el\\'"))))

;; @dev print object with text properties
(defun my-princ-with-properties (object properties &optional printcharfun)
  (let ((p (point)))
    (princ object printcharfun)
    (add-text-properties
      p (point)
      properties)))

;; @dev try run program, return error if failed
(defun my-try-run (program &rest args)
  (unless (= (apply 'call-process program nil "my-try-run" nil args) 0)
    (switch-to-buffer "my-try-run")
    (let ((output (buffer-string)))
      (kill-buffer (current-buffer))
      (error "running program \"%s\" returns failed, it outputs
             :\n%s" program output)))
  (kill-buffer (get-buffer "my-try-run")))

;; go to char (from wang ying)
(defun my-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `my-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
           char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(defun my-point-to-register()
  "Store cursorposition _fast_ in a register. Use
my-jump-to-register to jump back to the stored position."
  (interactive)
  (point-to-register 8))

(defun my-jump-to-register()
  "Switches between current cursorposition and position that was
stored with my-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
