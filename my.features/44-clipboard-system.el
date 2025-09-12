;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package browse-kill-ring
  :pin manual)

;; clipboard tweaks
; Non-nil means cutting and pasting uses the clipboard.
(setq select-enable-clipboard t)
; If non-nil, copy to kill ring upon mouse adjustments of the region.
(setq mouse-drag-copy-region nil)
