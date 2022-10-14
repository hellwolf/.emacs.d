(require 'rcirc)

;; https://www.gnu.org/software/emacs/manual/html_mono/rcirc.html#Fighting-Information-Overload
(add-hook 'rcirc-mode-hook
  (lambda ()
    (flyspell-mode 1)
    (rcirc-track-minor-mode 1)))
