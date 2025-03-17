(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; theme
(use-package zenburn-theme
  :pin manual
  :custom
  (zenburn-override-colors-alist
   '(("zenburn-bg+1"  . "#2F2F2F")
     ("zenburn-bg+2"  . "#3F3F3F")
     ("zenburn-bg+3"  . "#4F4F4F")
     ("zenburn-bg+05" . "#282828")))
  :init
  (load-theme 'zenburn t))

(custom-set-faces
  ;; '(term-color-blue ((t (:inherit default :background "royal blue" :foreground "royal blue"))))
  '(line-number-current-line ((t (:inherit default :foreground "orange")))))

;; fonts
(defun use-my-face-attributes (&optional frame)
  (let ((ff-def "Cascadia Code")
        (ff-math "Noto Sans Math")
        (ff-alt "FreeMono")
        (ff-color-emoji "Noto Color Emoji"))
    (set-face-attribute 'default nil :family ff-def)
    (set-face-attribute 'italic  nil :family ff-def :slant 'italic)

    ;; mapping unicode to font families
    ;; See - https://emacs.stackexchange.com/questions/62049/override-the-default-font-for-emoji-characters
    ;;     - https://en.wikipedia.org/wiki/Unicode_block
    ;; * Mathematical Symbols
    ;;   - https://en.wikipedia.org/wiki/Mathematical_Operators_(Unicode_block)
    (set-fontset-font t '(#x2200 . #x22FF) ff-math)
    ;;   - https://en.wikipedia.org/wiki/Miscellaneous_Mathematical_Symbols-A
    (set-fontset-font t '(#x27C0 . #x27ef) ff-math)
    ;;   - https://en.wikipedia.org/wiki/Miscellaneous_Mathematical_Symbols-B
    (set-fontset-font t '(#x2980 . #x29ff) ff-math)
    ;;   - https://en.wikipedia.org/wiki/Supplemental_Mathematical_Operators
    (set-fontset-font t '(#x2A00 . #x2Aff) ff-math)
    ;;   - https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols
    (set-fontset-font t '(#x1d400 . #x1d7ff) ff-math)
    ;; *  Misc Symbols
    ;;   - https://en.wikipedia.org/wiki/Box_Drawing
    ;;   - https://en.wikipedia.org/wiki/Block_Elements
    ;;   - https://en.wikipedia.org/wiki/Geometric_Shapes_(Unicode_block)
    (set-fontset-font t '(#x2500  .#x25ff) ff-alt)
    ;;   - https://en.wikipedia.org/wiki/Supplemental_Mathematical_Operators
    (set-fontset-font t '(#x2a00 . #x2aff) ff-alt)
    ;; * Emoji
    ;;   - https://en.wikipedia.org/wiki/Miscellaneous_Symbols
    (set-fontset-font t '(#x2600 . #x26ff) ff-color-emoji)
    ;;   - https://en.wikipedia.org/wiki/Emoticons_(Unicode_block)
    (set-fontset-font t '(#x1f000 . #x1f64f) ff-color-emoji)
  ))

;; default frame title
(setq frame-title-format "%f (%b)") ;; TODO is the file changed?

;; suppress welcome message
(setq inhibit-startup-message t)

;; disable some noises
(tool-bar-mode 0)

;; removing menu bar for terminal environment
;; https://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
     graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))
(add-hook 'after-make-frame-functions 'contextual-menubar)

;; setup hooks
(defun my-frame-setup-hook (&optional frame)
  (when window-system
    (use-my-face-attributes frame)
    (set-frame-size frame 130 40)))
(add-hook 'window-setup-hook 'my-frame-setup-hook)
(add-hook 'after-make-frame-functions 'my-frame-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; blink-cursor-mode
(blink-cursor-mode)

;; A utility package to collect various Icon Fonts and propertize them within Emacs
(use-package all-the-icons
  :pin manual
  :if (display-graphic-p))

;; set paren mode
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; color highlighting
(global-font-lock-mode t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mini Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show column number
(column-number-mode t)

;; Handy mode to make the modeline more succinct by allowing a diminished mode line string. Sometimes the fact that mode
;; is there is fine and it doesn't need to be on the mode line (diminish it to ""). Putting diminish first not out of
;; importance, but because it is used later on.
(use-package diminish
  :pin manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-<f11>") 'make-frame)
(global-set-key (kbd "C-x <menu>") 'toggle-menu-bar-mode-from-frame)
