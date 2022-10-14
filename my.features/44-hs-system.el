;; https://github.com/dongweiming/emacs.d/blob/master/hs-minor-mode-conf.el
(require 'hideshow)
(setq hs-hide-comments-when-hiding-all nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings (F3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fold/unfold, hide/show (Atom)
(global-set-key (kbd "C-M-[") 'hs-hide-block)
(global-set-key (kbd "C-M-]") 'hs-show-block)
(global-set-key (kbd "C-M-{") 'hs-hide-all)
(global-set-key (kbd "C-M-}") 'hs-show-all)
(global-set-key [(control shift mouse-1)] 'hs-mouse-toggle-hiding)
