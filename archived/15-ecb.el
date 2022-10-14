;;; ecb - cvs version
;(add-to-list 'load-path (my-catfile my-site-el-path "ecb-cvs"))
(require 'ecb)

;; hides the extra compile-window directly after the start of ECB
(defun my-ecb-activate-hook-func ()
  (ecb-toggle-compile-window -1)
  ;(add-hook 'find-file-hook 'ecb-rebuild-methods-buffer)
  )

(add-hook 'ecb-activate-hook 'my-ecb-activate-hook-func)

(setq ecb-vc-enable-support t)
(setq ecb-tip-of-the-day nil)
