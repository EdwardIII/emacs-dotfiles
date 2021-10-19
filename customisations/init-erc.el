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
  (setq znc-servers
       `((,(plist-get credentials :host) ,erc-port t
  	  ((libera ,(plist-get credentials :user)
  		   ,(decode-password credentials)))))))



(require 'erc)
(require 'erc-highlight-nicknames)

(setq erc-modules '(
  highlight-nicknames
  autojoin
  button
  completion
  fill
  irccontrols
  list
  log
  match
  menu
  move-to-prompt
  netsplit
  networks
  noncommands
  readonly
  ring
  stamp
  track
  ))

(erc-update-modules)

(setq erc-hide '("JOIN" "PART" "QUIT"))
(setq erc-kill-buffer-on-part t)

;;; Low distraction mode
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p) 
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "TOPIC"
                                    "324" "329" "332" "333" "353" "477"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

(defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
  ad-do-it
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))

(setq erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face))
;;; End Low distraction mode

(provide 'init-erc)
;;; init-erc.el ends here
