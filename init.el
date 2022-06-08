;;; init.el --- Initialization file for Emac.  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; cider is broken for us in daily, so get that from stable
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; For per-machine settings
(if (file-exists-p "~/.emacs.local.el")
    (load-file "~/.emacs.local.el"))


(require 'use-package-ensure) ; install every package if not already there
(setq ; use-package-always-defer t ; only load packages when they're used. stops :config being caled when you think it does
      use-package-always-ensure t
      use-package-verbose t)

(use-package vterm)
(use-package ag)
(use-package ace-window
  :init (global-set-key (kbd "M-o") 'ace-window))
(use-package nvm
  :hook
  (typescript-mode . nvm-use-for-buffer)
  ; because scss depends on a node interpreter
  (scss-mode . nvm-use-for-buffer))
(use-package php-mode)
(use-package magit
  :config
  ;;(setq auto-revert-buffer-list-filter
  ;;      'magit-auto-revert-repository-buffer-p) ;; speed up magit with tramp buffers
  )
(defvar compile-wcn? t) ; set with
(use-package js ; js-mode
  :hook (js-mode . (lambda () (add-hook 'before-save-hook #'ep--make-ui-vagrant-hook))))

(use-package elisp-format
  :init (require 'elisp-format))
(use-package cider
  :pin melpa-stable)
(use-package company
  :init (global-company-mode))


(use-package clojure-mode)
(use-package erc-hl-nicks)
(use-package cperl-mode
  :config
  (setq cperl-indent-level 2)
  (defalias 'perl-mode 'cperl-mode))
(use-package znc
  :init ) ;; TODO: Pull in from external file
;; TODO: Maybe this doesn't work in daemon mode because of the deferred loading, or because window-system is different under daemon-mode?
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize))
        (when (daemonp)
          (exec-path-from-shell-initialize)))
(use-package bufler
  :bind ("C-x C-b" . 'bufler))
(use-package json-mode
  :hook (json-mode . flycheck-mode))
(use-package ivy
  :init
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper-isearch))
(use-package smex
  :bind (("M-x" . 'smex)
         ("M-X" . 'smex-major-mode-commands)))
(use-package flycheck
  :init (global-flycheck-mode)
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checker '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package counsel
  :init
  (counsel-mode)
  :bind (("C-c g" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   ( "C-c k" . 'counsel-ag)))
(use-package which-key
  :init (which-key-mode))
(use-package tt-mode
  :init
  (autoload 'tt-mode "tt-mode")
  (setq auto-mode-alist
      (append '(("\\.tt$" . tt-mode))  auto-mode-alist )))
(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-mode-line "Projectile")
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
				    :compile "npm install"
				    :test "npm test"
				    :run "npm start"
				    :test-suffix ".spec.ts")
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)))
(use-package solarized-theme  ;; https://github.com/bbatsov/solarized-emacs
  :init
  (load-theme 'solarized-dark t)
  (set-terminal-parameter nil 'background-mode 'dark)
  (set-frame-parameter nil 'background-mode 'dark))
(use-package typescript-mode
  :after (flycheck-mode)
  :init
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package tide
  :preface   (defun setup-tide-mode ()
               (tide-setup)
               (flycheck-mode +1)
               (setq flycheck-check-syntax-automatically '(save mode-enabled))
               (eldoc-mode +1)
               (tide-hl-identifier-mode +1)
               (company-mode +1))
  :mode ("\\.ts" . typescript-mode)
  :hook ((typescript-mode . setup-tide-mode)
         (ng2-ts-mode . setup-tide-mode))
  :bind (("C-c t f" . tide-fix)
         ("C-c t o" . tide-format))
  :config
  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (setq tide-format-options '(
			      :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
			      :placeOpenBraceOnNewLineForFunctions nil
			      :indentSize 2
			      :tabSize 2
			      :placeOpenBraceOnNewLineForFunctions nil
			      :placeOpenBraceOnNewLineForControlBlocks nil)))
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "C-c l")
  
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         ;(perl-mode . lsp)
         
  :init
  (setq lsp-enable-snippet nil) ;; try disabling for now as getting weird indentation problems
  (setq lsp-enable-indentation nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf))
  ;;(lsp-register-client
  ;; (make-lsp-client :new-connection (lsp-tramp-connection '("perl-lsp"))
  ;;                  :major-modes '(perl-mode cperl-mode)
  ;;                  :remote? t
  ;;                  :server-id 'perl-ls)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-metals)

(use-package yasnippet)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package guru-mode
  :init
  (guru-global-mode +1))

(add-to-list 'load-path "~/.emacs.d/customisations")
(load "sexpers.el")
;(load "clipboard.el")

(load "init-shell.el")
(load "init-tide.el")
(load "init-erc.el")
;(load "init-mu4e.el")

(set-face-attribute 'default nil :height 140)

(global-set-key (kbd "C-<return>") 'set-mark-command)

(global-linum-mode)
(blink-cursor-mode 0)

(setq gc-cons-threshold 100000000) ;; 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq backup-directory-alist
          `((".*" . ,(concat user-emacs-directory "backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups/") t)))
(setq inhibit-startup-screen t)
(setq ediff-split-window-function 'split-window-sensibly)
(setq ediff-ignore-similar-regions t)
(setq js-indent-level 2)
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)

;; UI tweaks

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(windmove-default-keybindings)
(winner-mode)
(repeat-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      ;visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "~/.emacs.d/custom.el"))

(load custom-file)

;; I don't want ng2-ts-mode, only ng2-html-mode, so I force typescript-mode
;; to stop ng2-ts-mode from taking over .ts files
;(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))

(set-fontset-font t 'symbol 
                  (font-spec :family "Apple Color Emoji") 
                  nil 'prepend)

;; Makes emoji work on MacOS
(set-fontset-font t '(#x1f000 . #x1faff) (font-spec :family "Apple Color Emoji"))

;; Speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))

;; Faster than the default scp (for small files)

(setq tramp-inline-compress-start-size 1000)
(setq tramp-copy-size-limit 10000)
(setq vc-handled-backends '(Git))
(setq tramp-verbose 0)
(setq tramp-use-ssh-controlmaster-options nil)
(setq projectile--mode-line "Projectile")

(with-eval-after-load "tramp" (add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:user@host:")
                   "direct-async-process" t)))

(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

(provide 'init)
;;; init.el ends here

(fset 'fix-next-tide-error
   (kmacro-lambda-form [?\M-x ?f ?l ?y ?c ?h return ?\C-c ?t ?f] 0 "%d"))
