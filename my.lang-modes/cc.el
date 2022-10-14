(require 'cc-mode)
(require 'my-lib)

(setq c-default-style "stroustrup")
(setq-default c-basic-offset 4)

(dolist (m '(c-mode-hook c++-mode-hook))
  (add-hook m (lambda ()
                (c-set-offset 'arglist-intro '+)
                (c-set-offset 'innamespace 0)
                )))

;; auto hungry mode
(c-toggle-auto-hungry-state t)

;; associate hpp files with c++-mode
(setq auto-mode-alist
  (append '(("\\.hpp$" . c++-mode))
    auto-mode-alist))

(push (make-my-lang-mode :to-hook 'c-mode-hook) my-prog-lang-modes)
(push (make-my-lang-mode
        :to-hook 'c++-mode-hook
        :org-babel-lang 'C)
  my-prog-lang-modes)
