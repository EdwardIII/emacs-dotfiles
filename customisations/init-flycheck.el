;;; init.el --- Initialization file for Emac.
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(global-flycheck-mode)
(setq-default flycheck-disabled-checker '(emacs-lisp-checkdoc))
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'flycheck)
;;; flycheck.el ends here
