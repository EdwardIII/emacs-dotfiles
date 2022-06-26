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

;; Load before packages are started - fixes issues where
;; these vars are required at startup time
(add-to-list 'load-path "~/.emacs.d/customisations")
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      ;apropos-do-all t
      require-final-newline t
      ;visible-bell t
      load-prefer-newer t
      ;ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

(use-package vterm
  :config
  :bind (("C-c v" . vterm)))
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
(use-package cperl-mode
  :config
  (setq cperl-indent-level 2)
  (defalias 'perl-mode 'cperl-mode))
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize))
        (when (daemonp)
          (exec-path-from-shell-initialize)))
(use-package bufler
  :bind (("C-x C-b" . bufler)))
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
  (declare-function projectile-register-project-type "projectile")
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

(declare-function flycheck-add-mode "flycheck")
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
         (ruby-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         ;(perl-mode . lsp)

  :config
  (setq lsp-enable-snippet nil) ;; try disabling for now as getting weird indentation problems
  (setq lsp-enable-indentation nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))
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

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter))

(use-package yafolding
  :config
  (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-return>") nil)
  (define-key yafolding-mode-map (kbd "C-c <C-M-return>") nil)
  (define-key yafolding-mode-map (kbd "C-c <C-S-return>") nil)
  (define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)
  (add-hook 'json-mode-hook (lambda ()
                              (yafolding-mode))))

;; For some reason this fails on a mac with this error:
;; run-hooks: Autoloading file /Users/edward/.emacs.d/elpa/ws-butler-20201117.1528/ws-butler.elc failed to define function ws-butler
(unless (eq window-system 'ns)
  (use-package
    ws-butler
    :hook prog-mode))

(load "sexpers.el")
(load "init-shell.el")
(load "init-tide.el")
(load "init-irc.el")
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
(setq js-indent-level 2)

(when (eq window-system 'ns)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font t '(#x1f000 . #x1faff) (font-spec :family "Apple Color Emoji")))

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
(global-set-key(kbd "C-M-r") 'isearch-backward)

;; I don't want ng2-ts-mode, only ng2-html-mode, so I force typescript-mode
;; to stop ng2-ts-mode from taking over .ts files
;(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))


;; Speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))

(require 'tramp-sh)
(setq tramp-inline-compress-start-size 1000)
(setq tramp-copy-size-limit 10000)
(setq vc-handled-backends '(Git))
(setq tramp-verbose 0)
(setq tramp-use-ssh-controlmaster-options nil)
(setq projectile--mode-line "Projectile")

(with-eval-after-load "tramp" (add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:user@host:")
                   "direct-async-process" t)))

(require 'browse-url)
(setq browse-url-generic-program (if (eq window-system 'ns) "open" "firefox")
      browse-url-browser-function 'browse-url-generic)

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode t)
(add-hook 'emacs-lisp-mode-hook 'hs-hide-initial-comment-block t)

(require 'org)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(require 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))

(fset 'fix-next-tide-error
      (kmacro-lambda-form [?\M-x ?f ?l ?y ?c ?h return ?\C-c ?t ?f] 0 "%d"))

(require 'ffap)
(defun browse-last-url-in-brower ()
  "Find the last url in the buffer."
  (interactive)
  (save-excursion
    (ffap-next-url t t)))

;; TODO: eh?
(global-set-key (kbd "C-c u") #'ffap-next-url)

(defun ep/find-files (path criteria)
  "Search PATH for all files matching CRITERIA, a glob."
  (process-lines "find" path "-name" criteria))

(defun ep/clean-file-whitespace (filename)
  "Clean up all whitespace in this FILENAME.
Opens the file, changes it, saves it, then closes the buffer."
  (find-file filename)
  (goto-char (point-max))
  (activate-mark)
  (whitespace-cleanup-region (point-min) (point-max))
  (save-buffer)
  (kill-buffer))

(defun ep/extension->glob ()
  "For the current buffer, turn the exexion into a glob.
E.g. init.el -> *.el"
  (if-let ((filename buffer-file-name))
      (format "*.%s" (file-name-extension (buffer-file-name)))
    (progn
      (message "Tried to get extension buffer is not a file")
      "")))

(defun ep/fix-whitespace-in (directory criteria)
  "Recursively cleanup whitespace issues.
Recursively searches through DIRECTORY for CRITERIA (glob)."
  (interactive (list (read-string "Directory? " (read-directory-name "."))
                     (read-string "Criteria (glob)? " (ep/extension->glob))))
  (mapc 'ep/clean-file-whitespace (ep/find-files directory criteria)))


(require 'xref)
(require 'project)

(defun ep/find-files-ag (regex directory)
  "Search in DIRECTORY for REGEX."
  ;; TODO: not doing ignores properly, they are magically coming
  ;; from somewhere
  (process-lines-ignore-status "ag" "-l" regex (expand-file-name directory)))

(defun ep/-append-path (path &optional optional-path)
  "Append OPTIONAL-PATH to PATH, if OPTIONAL-PATH is set."
  (if optional-path
      (concat (file-name-as-directory path) (file-name-as-directory optional-path))
    path))

(defun ep/-project-file-ag
    (regex &optional subdir)
  "Search for the REGEX in the project directory.
Optional SUBDIR to limit searches to a certain directory"
  (let ((regex regex)
        (path (or subdir (ep/-append-path
                           (project-root
                            (project-current))
                           subdir))))
    (if-let ((found-files
                (ep/find-files-ag regex
                                  path)))
      (xref--show-xrefs
       (xref-matches-in-files regex
                              found-files)
       nil)
      (user-error "No files found"))))

(defun ep/project-file-ag
    (regex &optional subdir)
  "Search for the REGEX in the project directory.
Optional SUBDIR to limit searches to a certain directory"
  (interactive (list (read-string "Regex: ")
                     (when current-prefix-arg (read-directory-name "Sub directory: " "."))))
  (ep/-project-file-ag regex subdir))

(global-set-key (kbd "C-c f f") 'ep/project-file-ag)

;;; Ever need to turn off vc to speed up ssh tramp access?
;; (with-eval-after-load 'vc
;;   (remove-hook 'find-file-hook 'vc-find-file-hook)
;;   (remove-hook 'find-file-hook 'vc-refresh-state)
;;   (setq vc-handled-backends nil))


(provide 'init)
;;; init.el ends here
