(require 'rcirc)

(use-package rcirc ;; no-nixpkgs
  :pin manual
  :custom
  (rcirc-reconnect-delay 60)
  (rcirc-time-format "%m-%d %H:%M ")
  (rcirc-fill-column 100)
  (rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (rcirc-omit-unless-requested '("NAMES"))
  :hook (rcirc-mode . #'my-rcirc-mode-hook)
)

(defun my-rcirc-mode-hook ()
  (setq truncate-lines nil)
  (flyspell-mode)
  (rcirc-omit-mode)
  (rcirc-track-minor-mode)
)
