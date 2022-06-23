;;; init-irc.el --- Setup Circe with nick highlighting and my own credentials  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Edward Prendergast

;; Author: Edward Prendergast <edward@MacBook-Pro-2.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'use-package)

(setq-local irc-port "1337")

(defun ep/decode-password (credentials)
  "Provide a decoded plain text password.
Given CREDENTIALS from auth-source-search."
  (funcall (plist-get credentials
                      :secret)))

(defun ep/credentials (port)
  "Lookup username/password credentials.
Find by unique PORT."
  (nth 0 (auth-source-search :max 1
                             :port port
                             :require '(:user
                                        :secret
                                        :port
                                        :host))))

(defun ep/circe-credentials ()
  "Get network config for circe-network-options based on local saved credentials."
  (let* ((credentials (ep/credentials irc-port))
         (username (plist-get credentials :user))
         (password (ep/decode-password credentials))
         (user-and-pass (format "%s/libera:%s" username password)))
    `(("edwardiii.co.uk" :tls t
       :nick "edwardiii"
       :sasl-username ,username
       :sasl-password ,user-and-pass
       :pass ,user-and-pass
       :port ,irc-port))))

(use-package circe
  :init
  (setq circe-network-options (ep/circe-credentials))
  (enable-circe-color-nicks)
  :config
  ;; https://github.com/emacs-circe/circe/issues/298
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore))


(use-package circe-notifications
  :config
  (autoload 'enable-circe-notifications "circe-notifications" nil t)

  (eval-after-load "circe-notifications"
    '(setq circe-notifications-watch-strings
           '("EdwardIII")
           circe-notifications-alert-style (if (eq window-system 'ns)
                                               'osx-notifier
                                             'libnotify)))

  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

(provide 'init-irc)
;;; init-irc.el ends here
