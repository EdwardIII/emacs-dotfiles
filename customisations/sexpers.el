;;; init.el --- Initialization file for Emac.
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(show-paren-mode 1)

;; ;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
;; ;; This is useful for working with camel-case tokens, like names of
;; ;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

(autoload 'fennel-mode "fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(eval-after-load 'fennel-mode
  '(define-key fennel-mode-map (kbd "C-c C-k")
     (defun pnh-fennel-hotswap ()
       (interactive)
       (comint-send-string
        (inferior-lisp-proc)
        (format "(lume.hotswap \"%s\")\n"
                (substring (file-name-nondirectory (buffer-file-name)) 0 -4))))))

(add-hook 'prog-mode-hook
          (defun before-fennel-mode()
            (when (eq major-mode 'fennel-mode)
              (setq-local inferior-lisp-program "love ."))))

(provide 'sexpers)
;;; sexpers.el ends here

