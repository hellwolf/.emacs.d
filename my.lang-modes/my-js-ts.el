(require 'use-package)
(require 'my-lib)

(defun eslint-fix-file-and-revert ()
  (interactive)
  ;; (message "npx eslint --fixing the file %s" (buffer-file-name))
  (shell-command (concat "npx eslint --fix " (buffer-file-name)))
  (revert-buffer t t))

(defun my-js-ts-modes-hook ()
  ;; buffer local
  (add-hook 'after-save-hook #'eslint-fix-file-and-revert nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  ;;
  :mode (("\\.js\\'"   . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode))

;; org-babel-js fix https://gist.github.com/mrspeaker/c3b7b8d0b0b96b1a012d736b22d12b2e
(defvar org-babel-js-function-wrapper
  "console.log(JSON.stringify(require('util').inspect(function(){\n%s\n}())));"
  "Javascript code to print value of body.")

(push (make-my-lang-mode
        :to-hook        'js2-jsx-mode-hook
        :my-hook        #'my-js-ts-modes-hook
        :org-babel-lang 'js)
  my-prog-lang-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode babel extension for typescript
(use-package ob-typescript)

(use-package typescript-mode)

(push (make-my-lang-mode
        :to-hook        'typescript-mode-hook
        :my-hook        #'my-js-ts-modes-hook
        :org-babel-lang 'typescript)
  my-prog-lang-modes)
