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

(setq erc-hide '("INVITE"
                 "JOIN"
                 "KICK"
                 "MODE"
                 "NICK"
                 "PART"
                 "PING"
                 "PONG"
                 "PRIVMSG"
                 "NOTICE"
                 "QUIT"
                 "TOPIC"
                 "WALLOPS"
                 "MOTD"
                 "371"
                 "372"
                 "374"
                 "375"
                 "376"
                 "422"
                 "004"
                 "005"
                 "221"
                 "252"
                 "253"
                 "254"
                 "250"
                 "251"
                 "255"
                 "256"
                 "257"
                 "258"
                 "259"
                 "265"
                 "266"
                 "377"
                 "378"
                 "275"
                 "290"
                 "301"
                 "303"
                 "305"
                 "306"
                 "307"
                 "311"
                 "314"
                 "312"
                 "313"
                 "315"
                 "318"
                 "323"
                 "369"
                 "317"
                 "319"
                 "320"
                 "321"
                 "322"
                 "324"
                 "328"
                 "329"
                 "330"
                 "331"
                 "341"
                 "352"
                 "353"
                 "366"
                 "367"
                 "368"
                 "379"
                 "391"
                 "401"
                 "403"
                 "404"
                 "405"
                 "406"
                 "412"
                 "421"
                 "432"
                 "433"
                 "437"
                 "442"
                 "461"
                 "465"
                 "474"
                 "475"
                 "477"
                 "482"
                 "671"
                 "431"
                 "445"
                 "446"
                 "451"
                 "462"
                 "463"
                 "464"
                 "481"
                 "483"
                 "484"
                 "485"
                 "323"
                 "364"
                 "365"
                 "381"
                 "382"
                 "392"
                 "393"
                 "394"
                 "395"
                 
                 "JOIN" "NICK" "PART" "QUIT" "MODE" "TOPIC"
                 "324" "329" "332" "333" "353" "477"))

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

(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p) 
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
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


(provide 'init-erc)
;;; init-erc.el ends here
