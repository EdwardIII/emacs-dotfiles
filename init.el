;;; init.el --- Initialization file for Emac.

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; cider is broken for us in daily, so get that from stable
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  ;; Download the theme manually and put it in ~/.emacs.d/themes/
  ;;  https://github.com/sellout/emacs-color-theme-solarized
  
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
    company
 
    counsel

    ivy

    forge

    ng2-mode

    znc))

(dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/customisations")
(load "theme.el")
(load "sexpers.el")
(load "clipboard.el")
(load "init-flycheck.el")
(load "init-shell.el")
(load "init-tide.el")

(load "init-erc.el")

(with-eval-after-load 'magit
  (require 'forge)
  (setq auth-sources '("~/.authinfo"))
  (setq forge-add-pullreq-refspec 'ask forge-pull-notifications t
                 forge-topic-list-limit '(60 . 0)))


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
(global-set-key (kbd "C-<return>") 'set-mark-command)


(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec.ts")


(global-linum-mode)

(setq backup-directory-alist
          `((".*" . ,(concat user-emacs-directory "backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups/") t)))

(setq inhibit-startup-screen t)
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

(setq ediff-split-window-function 'split-window-sensibly)
(setq ediff-ignore-similar-regions t)

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)


;; I don't want ng2-ts-mode, only ng2-html-mode, so I force typescript-mode
;; to stop ng2-ts-mode from taking over .ts files

(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))

(add-hook 'prog-mode-hook
          (defun before-ng2-ts-mode()
            (when (eq major-mode 'ng2-ts-mode)
              (typescript-mode))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(znc clojure-mode-extra-font-locking paredit cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
