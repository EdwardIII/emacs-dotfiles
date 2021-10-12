;;; init.el --- Initialization file for Emac.
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(require 'package)

;; Add melpa package source when using package list
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load emacs packages and activate them
;; This must come before configurations of installed packages.
;; Don't delete this line.
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  ;; Download this one manually and put it in ~/.emacs.d/themes/
  ;;  https://github.com/sellout/emacs-color-theme-solarized
  ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
  '(paredit
    exec-path-from-shell
    
    clojure-mode

    ;; extra syntax highlighting for clojure
    ;; clojure-mode-extra-font-locking
 
    cider

    flycheck

    magit
    
    which-key

    smex

    projectile

    tide

    counsel

    ivy

    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/customisations")
(load "theme.el")
(load "sexpers.el")
(load "clipboard.el")
(load "init-flycheck.el")
(load "init-shell.el")
(load "init-tide")

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(which-key-mode)
(set-face-attribute 'default nil :height 140)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)

(counsel-mode)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)

(setq js-indent-level 2)

(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(global-linum-mode)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq inhibit-startup-screen t)
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(clojure-mode-extra-font-locking paredit cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
