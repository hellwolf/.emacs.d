(require 'rcirc)

;; https://www.gnu.org/software/emacs/manual/html_mono/rcirc.html#Fighting-Information-Overload
(add-hook 'rcirc-mode-hook
  (lambda ()
    (flyspell-mode 1)
    (rcirc-omit-responses 1)
    (setopt rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
    (rcirc-track-minor-mode 1)))
