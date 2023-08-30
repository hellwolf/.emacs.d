(require 'use-package)
(require 'my-lib)

(use-package cmake-mode
  :mode "CMakeLists.txt")

(push (make-my-lang-mode
        :to-hook 'cmake-mode-hook)
  my-prog-lang-modes)
