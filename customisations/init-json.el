;;; init-json.el --- Work with JSON                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Edward Prendergast

;; Author: Edward Prendergast;; <edward@EddBook.local>
;; Keywords: data

;;; Commentary:

;; Starts up json-mode and adds our own function to
;; speed up unescaping JSON text that's been copied
;; and pasted in from log entries.
;;
;; Requires the json-mode package from melpa

;;; Code:

(add-hook 'json-mode-hook #'flycheck-mode)
(add-hook 'json-mode-hook (lambda () (local-set-key  (kbd "C-c C-j") 'unescape-json)))

(defun unescape-json (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert (json-read-from-string regionp))))))

(provide 'init-json)
;;; init-json.el ends here


