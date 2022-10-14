(setq emacsprj-loaded 0)

(setq emacsprj-autoconfig-options (makehash))

;; load emacsprj
(defun emacsprj-load (sourcedir)
  (interactive "DLoad emacs project from directory: %s" sourcedir)

  ;; Load more than one project is not supported
  (if (eq emacsprj-loaded 1)
      (error "Emacsprj is already loaded"))

  ;;
  ;; basic emacsprj variables
  ;;
  (setq emacsprj-top-directory
        (replace-regexp-in-string  "/*$" ""
                                   (file-truename sourcedir)))
  (setq emacsprj-id (file-name-nondirectory emacsprj-top-directory))
  (setq emacsprj-control-directory
        (my-catfile emacsprj-top-directory ".emacsprj"))
  (setq emacsprj-data-directory
        (my-catfile emacsprj-control-directory "data"))
  (setq emacsprj-autoconfig-file
        (my-catfile emacsprj-data-directory "autoconfig.el"))
  (setq emacsprj-config-file
        (my-catfile emacsprj-control-directory "config.el"))
  (setq emacsprj-utility-bin
        (my-catfile my-tools-path "emacsprj-utility"))

  ;;
  ;; check sanity of emacs project
  ;;
  ;;; check emacs project control directory
  (unless (file-accessible-directory-p emacsprj-control-directory)
    (error "Emacsprj control directory \"%s\" doesn't exist"
           emacsprj-control-directory))
  ;; autoconfig
  (my-try-run emacsprj-utility-bin emacsprj-top-directory "autoconfig")
  (load emacsprj-autoconfig-file)

  ;;
  ;; loading project environment
  ;;
  ;;; eshell
  (setq eshell-directory-name (my-catfile emacsprj-data-directory "eshell"))
  ;;; recentf
  (setq recentf-save-file (my-catfile emacsprj-data-directory "recentf"))
  ;;; ido
  (setq ido-save-directory-list-file (my-catfile emacsprj-data-directory "ido.last"))
  ;;; desktop
  (desktop-change-dir emacsprj-data-directory)
  (desktop-save emacsprj-data-directory)
  (desktop-save-mode)
  ;;; bookmarks
  (setq bookmark-default-file (my-catfile emacsprj-data-directory "bookmarks"))
  (add-hook 'kill-emacs-hook 'bookmark-save)
  ;;; session
  (setq session-save-file (my-catfile emacsprj-data-directory ".session"))
  (session-initialize)
  ;;; start emacs server mode
  (setq server-name emacsprj-id)
  (server-start)
  ;;; misc configurations
  (setq default-directory emacsprj-top-directory)
  ;;; user configuration
  (if (file-readable-p emacsprj-config-file)
      (load emacsprj-config-file))

  ;;; initialize ecb
  (ecb-activate)

  ;;
  ;; post operations
  ;;
  (emacsprj-load-refs)
  (setq emacsprj-loaded 1)
  (message "Emacsprj %s loaded" emacsprj-id)
)

;; sync datas of emacsprj. Such as tags, cscope db, ebrowse db, etc.
(defun emacsprj-sync ()
  (interactive)
  (if (eq emacsprj-loaded 1)
      (progn
        (my-try-run emacsprj-utility-bin emacsprj-top-directory "sync")
        (emacsprj-load-refs)
        (message "Emacsprj %s synced" emacsprj-id))
    (error "Emacsprj is not loaded")))

(defun emacsprj-load-refs ()
  (if (gethash 'EMACSPRJ_ENABLE_TAGS emacsprj-autoconfig-options)
      (let ((tags-revert-without-query t))
          (visit-tags-table emacsprj-data-directory)))
  (if (gethash 'EMACSPRJ_ENABLE_CSCOPE emacsprj-autoconfig-options)
      (setq cscope-initial-directory emacsprj-data-directory)))

(defun emacsprj-browse ()
  (interactive)
  (if (gethash 'EMACSPRJ_ENABLE_EBROWSE emacsprj-autoconfig-options)
	(switch-to-buffer (find-file-noselect
  		(my-catfile emacsprj-data-directory "BROWSE")))))

(dolist (m '("/usr/share/cscope/xcscope.el"
             "/usr/share/emacs/site-lisp/xcscope.el"))
  (if (file-accessible-directory-p m)
    (load m)))
(setq cscope-do-not-update-database t)