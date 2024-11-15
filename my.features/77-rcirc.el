(require 'rcirc)

(use-package rcirc
  :pin manual
  :custom
  (truncate-lines nil)
  (rcirc-reconnect-delay 60)
  (rcirc-fill-column 80)
  (rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  :hook (rcirc-mode . my-rcirc-mode-hook)
)

(defun my-rcirc-mode-hook ()
  (flyspell-mode)
  (rcirc-omit-mode)
  (rcirc-track-minor-mode)
)
