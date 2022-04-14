;; -*- lexical-binding: t; -*-

(use-package load-dir)

;; Must `brew install mu fetchmail` for email support separately
(require 'load-dir)
(load-dir-one "/usr/local/Cellar/mu/1.6.8/share/emacs/site-lisp")

; used by mu4e to show images
(imagemagick-register-types)

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Bin")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( (:maildir "/INBOX"              :key ?i)
       (:maildir "/[Gmail].Sent Mail"  :key ?s)
       (:maildir "/[Gmail].Bin"      :key ?t)
       (:maildir "/[Gmail].All Mail"   :key ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "edward@openedweb.co.uk"
   user-full-name  "Edward Prendergast"
   mu4e-compose-signature
    (concat
      "Edward Prendergast\n"
      "http://www.openedweb.co.uk\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
 
(setq mu4e-view-show-images t
       mu4e-show-images t)
(imagemagick-register-types)

(provide 'init-mu4e)

;;; init-mu4e.el ends here
