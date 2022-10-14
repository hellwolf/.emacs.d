(require 'use-package)
(require 'evil-core nil t)

(use-package evil
  :custom
  ;; using undo-tree
  (evil-undo-system 'undo-tree)
  ;; evil search
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; stick to emacs bindings in evil insert mode
  (evil-disable-insert-state-bindings t)
  ;; evil cursors
  (evil-normal-state-cursor 'hollow)
  (evil-insert-state-cursor '(bar . 1))
  (evil-emacs-state-cursor 'box)
  ;; default to emacs mode except for prog/text modes
  (evil-default-state 'emacs)
  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (evil-symbol-word-search t)
  :config
  ;; Remap to emacs movement keybindings in normal mode
  (define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
  (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  :init
  (evil-mode))

;; https://github.com/doomemacs/doomemacs/issues/1994
(use-package evil-terminal-cursor-changer
  :hook
  (tty-setup . evil-terminal-cursor-changer-activate))
