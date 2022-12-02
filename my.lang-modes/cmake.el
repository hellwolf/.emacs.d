(require 'use-package nil)
(require 'my-lib)

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

(push (make-my-lang-mode
        :to-hook 'cmake-mode-hook)
  my-prog-lang-modes)
