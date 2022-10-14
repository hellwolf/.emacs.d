(defun my-setup-i18n ()
  (prefer-coding-system 'utf-8)

  (set-default-font "Liberation Mono-9")
  ;; set cjk font for fontset-default
  (let ((fontname '("WenQuanYi Bitmap Song" . nil)))
	(dolist (elt `((unicode . ,fontname)
				   (chinese-gb2312 . ,fontname)
				   (chinese-big5-1 . ,fontname)
				   (chinese-big5-2 . ,fontname)
				   (chinese-sisheng . ,fontname)
				   (chinese-cns11643-1 . ,fontname)
				   (chinese-cns11643-2 . ,fontname)
				   (chinese-cns11643-3 . ,fontname)
				   (chinese-cns11643-4 . ,fontname)
				   (chinese-cns11643-5 . ,fontname)
				   (chinese-cns11643-6 . ,fontname)
				   (chinese-cns11643-7 . ,fontname)
				   (japanese-jisx0208-1978 . ,fontname)
				   (japanese-jisx0208 . ,fontname)
				   (japanese-jisx0212 . ,fontname)
				   (japanese-jisx0213-1 . ,fontname)
				   (japanese-jisx0213-2 . ,fontname)
				   (korean-ksc5601 . ,fontname)))
	  (set-fontset-font "fontset-default" (car elt) (cdr elt)))))

(my-setup-i18n)

(add-hook 'after-make-frame-functions (lambda (frame) (select-frame frame) (my-setup-i18n)) t)

;; set sample texts for "Display Faces"
(setq list-faces-sample-text
	  "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 11223344556677889900
	  ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 壹贰叁肆伍陆柒捌玖零")
