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
    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    ;; clojure-mode-extra-font-locking
 
    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    flycheck
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
;; ;; A little more syntax highlighting

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

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

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(global-linum-mode)

(global-flycheck-mode)
(setq-default flycheck-disabled-checker '(emacs-lisp-checkdoc))
(add-hook 'after-init-hook #'global-flycheck-mode)

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
