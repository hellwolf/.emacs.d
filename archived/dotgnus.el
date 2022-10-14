(setq gnus-select-method '(nntp "news.yaako.com"))
(setq gnus-message-archive-method
      `(nnfolder "archive"
                 (nnfolder-directory ,(concat gnus-directory "archive"))
                 (nnfolder-active-file ,(concat gnus-directory "archive/active"))
                 (nnfolder-get-new-mail nil)
                 (nnfolder-inhibit-expiry t)))

;;prevent teared threads
(setq gnus-fetch-old-headers 'some)

;;layout
;(gnus-add-configuration 
; '(article (vertical 1.0
;                     (summary .35 point)
;                     (article 1.0))))

;;mygroup
(setq gnus-message-archive-group
      '((if (message-news-p)
            "mail.sent.news"
          "mail.sent.mail")))

;;charset
(setq gnus-group-posting-charset-alist
	'(("^cn\\." gb2312 (gb2312))))

