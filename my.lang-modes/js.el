(require 'use-package)
(require 'my-lib)

(defun eslint-fix-file ()
  (interactive)
  (message "npx eslint --fixing the file %s" (buffer-file-name))
  (shell-command (concat "npx eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(defun my-jsxx-mode-hook ()
  ;; buffer local
  (add-hook 'after-save-hook #'eslint-fix-file-and-revert nil t))

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package ob-typescript)

;; org-babel-js fix https://gist.github.com/mrspeaker/c3b7b8d0b0b96b1a012d736b22d12b2e
(defvar org-babel-js-function-wrapper
  "console.log(JSON.stringify(require('util').inspect(function(){\n%s\n}())));"
  "Javascript code to print value of body.")

(push (make-my-lang-mode
        :to-hook        'js2-jsx-mode-hook
        :my-hook        #'my-jsxx-mode-hook
        :org-babel-lang 'js)
  my-prog-lang-modes)

(push (make-my-lang-mode
        :to-hook        'typescript-mode-hook
        :my-hook        #'my-jsxx-mode-hook
        :org-babel-lang 'typescript)
  my-prog-lang-modes)
