;;; init.el --- Initialization file for Emac.
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(defun copy-from-osx ()
  "Paste from OSX clipboard."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text)
  "Copy from OSX clipboard.
The argument TEXT contains the text you want to send to the OSX buffer"
  
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (eq system-type 'darwin)
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))


(provide 'clipboard)
;;; clipboard.el ends here
