(require 'use-package)
(require 'my-lib)

;; org-reveal
;; repo: https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :pin manual
  :after (org htmlize)
  :custom
  ;;(org-reveal-root (my-catfile (getenv "HOME") "Applications" "reveal.js"))
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@4.4.0"))
