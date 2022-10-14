(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  '(ansi-color-faces-vector
     [default default default italic underline success warning error])
  '(custom-enabled-themes (quote (tango-dark))))

(custom-set-faces
  '(line-number-current-line ((t (:foreground "orange"))))
  '(term-color-blue ((t (:background "royal blue" :foreground "royal blue")))))

(set-mouse-color "yellow")

(set-frame-font "Fira Code-10")
(set-face-attribute 'default nil :font "Fira Code-10")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blink-cursor-mode
(blink-cursor-mode)
;; A utility package to collect various Icon Fonts and propertize them within Emacs
;; (defvar all-the-icons)
(use-package all-the-icons
  :if (display-graphic-p))
;; set paren mode
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;; color highlighting
(global-font-lock-mode t)
;; highlight-symbol.el
(use-package highlight-symbol)
;; https://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)
;; truncate lines
(set-default 'truncate-lines t)
(setq-default show-trailing-whitespace t)
;; display full column indicator (emacs 27+)
(if (fboundp 'global-display-fill-column-indicator-mode)
  (global-display-fill-column-indicator-mode))
;; mark tweaks
(transient-mark-mode t)
(setq mark-even-if-inactive t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mini Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show column number
(column-number-mode t)
;; Handy mode to make the modeline more succinct by allowing a diminished mode line string. Sometimes the fact that mode
;; is there is fine and it doesn't need to be on the mode line (diminish it to ""). Putting diminish first not out of
;; importance, but because it is used later on.
(use-package diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default frame title
(setq frame-title-format "%f (%b)")
;; suppress welcome message
(setq inhibit-startup-message t)
;; disable some noises
(tool-bar-mode 0)
;; (menu-bar-mode 0)

;; https://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames
;; (defun contextual-menubar (&optional frame)
;;   "Display the menubar in FRAME (default: selected frame) if on a
;;     graphical display, but hide it if in terminal."
;;   (interactive)
;;   (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))
;; (add-hook 'after-make-frame-functions 'contextual-menubar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (<F11>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-<f11>") 'make-frame)
;; (global-set-key (kbd "<menu>") 'toggle-menu-bar-mode-from-frame)
