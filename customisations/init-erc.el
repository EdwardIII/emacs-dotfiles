;;; init-erc.el --- Pulls credentials from ~/.authinfo and sets up znc-servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Edward Prendergast

;; Author: Edward Prendergast;;; init-erc.el --- Configure znc.el                 -*- lexical-binding: t; -*- <edward@EddBook.local>

;;; Code:


;; Copyright (C) 2021  Edward Prendergast

;; Author: Edward Prendergast(require 'auth-source) <edward@EddBook.local>

(require 'znc)

;; assumes you use port 1337 for your bnc
(setq-local erc-port "1337")

(defun decode-password (credentials)
  (funcall (plist-get credentials :secret)))

;; Just in case you need to print this out:
;; (setq eval-expression-print-level 9999)

(let ((credentials (nth 0 (auth-source-search :max 1
					      :port erc-port
					      :require '(:user :secret :port :host)))))
  (set 'znc-servers
       `((,(plist-get credentials :host) ,erc-port t
  	  ((libera ,(plist-get credentials :user)
  		   ,(decode-password credentials)))))))

(provide 'init-erc)
;;; init-erc.el ends here
