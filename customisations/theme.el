;;; init.el --- Initialization file for Emac.
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

(provide 'theme)
;;; theme.el ends here
