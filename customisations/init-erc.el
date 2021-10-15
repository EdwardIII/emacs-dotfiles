(require 'auth-source)
(require 'znc)

(setq-local erc-port "1337")

(defun host (credentials)
  (plist-get credentials :host))

(defun user (credentials)
  (plist-get credentials :user))

(defun password (credentials)
  (funcall (plist-get credentials :secret)))

(defun credentials ()
  (nth 0 (auth-source-search :max 1
			      :port erc-port ;; assumes you use port 1337 for your bnc
			      :require '(:user :secret :port :host))))

;; Just in case you need to print this out:
;; (setq eval-expression-print-level 9999)

(set 'znc-servers
      `((,(host (credentials)) ,erc-port t
	 ((libera ,(user (credentials)) ,(password (credentials)))))))


