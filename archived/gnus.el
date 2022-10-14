;;
;; gnus base settings
;;

(require 'gnus)
(setq gnus-home-directory (concat my-data-path "gnus.d/"))
(setq gnus-directory gnus-home-directory)
(setq gnus-article-save-directory (concat gnus-directory "News"))
(setq gnus-kill-files-directory (concat gnus-directory "News"))
(setq gnus-cache-directory (concat gnus-directory "News/cache"))
(setq message-auto-save-directory (concat gnus-directory "Mail/drafts"))

