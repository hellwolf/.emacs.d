(require 'cperl-mode)
(require 'my-lib)

;; perl xs file
(setq auto-mode-alist
  (append '(("\\.xs$" . c-mode))
    auto-mode-alist))

;; perl mode for psh
(setq auto-mode-alist
  (append '(("\\.psh$" . perl-mode))
    auto-mode-alist))

;; indention
(setq cperl-indent-parens-as-block t
  cperl-tab-always-indent t)

(push (make-my-lang-mode
        :to-hook        'cperl-mode-hook
        :org-babel-lang 'perl)
  my-prog-lang-modes)
