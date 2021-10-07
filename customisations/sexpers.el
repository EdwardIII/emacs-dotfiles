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

(provide 'sexpers)
;;; sexpers.el ends here

