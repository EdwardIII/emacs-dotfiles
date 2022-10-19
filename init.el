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

;; Manually installed packages.
;; Usually put here because I need to patch them
(add-to-list 'load-path (concat user-emacs-directory "lisp/nord-emacs/" ))
(defun init-nord-theme (&optional _)
  "Initialise the nord theme."
  (load "nord-theme")
  (load-theme 'nord t))
;; https://emacs.stackexchange.com/a/3337
(add-hook 'after-make-frame-functions 'init-nord-theme)

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
  :config)

(use-package js)

(use-package elisp-format
  :init (require 'elisp-format))
(use-package cider
  :pin melpa-stable)
(use-package company
  :init (global-company-mode))

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

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
  :bind (("C-'" . 'avy-goto-char-2))
  :init
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper-isearch))
(use-package flycheck
  :init (global-flycheck-mode)
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checker '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; be sure to install the following to get scss support:
  ;; npm install -g npm install stylelint stylelint-scss stylelint-config-standard-scss
  ;; Then add something like the following to ~/.stylelintrc.json:
  ;; {
  ;;   "extends": "stylelint-config-standard-scss",
  ;;   "plugins": [
  ;;     "stylelint-scss"
  ;;   ],
  ;;   "rules": {
  ;;     "at-rule-no-unknown": null,
  ;;     "scss/at-rule-no-unknown": true
  ;;   }
  ;; }

  :config
  ;; Overwrite existing scss-stylelint checker to not use --syntax
  ;; as this breaks the latest version of stylelint
  ;; See: scss-stylelint checker doesn't work due to --syntax option removed from stylelint v14
  (flycheck-define-checker scss-stylelint
    "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
    :command ("stylelint"
              (eval flycheck-stylelint-args)
              ;; "--syntax" "scss"
              (option-flag "--quiet" flycheck-stylelint-quiet)
              (config-file "--config" flycheck-stylelintrc))
    :standard-input t
    :error-parser flycheck-parse-stylelint
    :modes (scss-mode)))
(use-package counsel
  :init
  (counsel-mode)
  :bind (("M-x" . 'counsel-M-x)
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

;; (use-package solarized-theme  ;; https://github.com/bbatsov/solarized-emacs
;;   :init
;;   (load-theme 'solarized-dark t)
;;   (set-terminal-parameter nil 'background-mode 'dark)
;;   (set-frame-parameter nil 'background-mode 'dark))

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
         ("C-c t o" . tide-format)
         ("C-c t r" . tide-rename-symbol))
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

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "C-c l")

  :hook
  (ruby-mode . lsp)
  (scss-mode . lsp) ;; don't forget to M-x lsp-install-server RET css-ls RET
  (json-mode . lsp)
  (lsp-mode . lsp-lens-mode)

  :config
  (setq lsp-enable-snippet nil) ;; try disabling for now as getting weird indentation problems
  (setq lsp-enable-indentation nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)

  (declare-function lsp-register-client "lsp-mode")
  (declare-function make-lsp-client "lsp-mode")
  (declare-function lsp-tramp-connection "lsp-mode")
  (declare-function lsp--set-configuration "lsp-mode")
  (declare-function lsp-configuration-section "lsp-mode")

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda ()
                                       (list "perl"
                                             "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run" "--"
                                             (format "--port %d --version %s"
                                                     13603 "2.1.0"))))
                    :major-modes '(perl-mode cperl-mode)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration
                                         (lsp-configuration-section "perl"))))
                    :priority -2
                    :server-id 'perl-language-server-remote
                    :remote? t)))

(use-package
  dap-mode
  :config (dap-auto-configure-mode)
  (require 'dap-node)
  (declare-function dap-register-debug-template "dap-mode")
  ;; You need to run (dap-node-setup), and also do this:
  ;; See https://github.com/emacs-lsp/dap-mode/issues/554
  (dap-register-debug-template "TS Scratch"
                               '(:type "node"
                                       :request "launch"
                                       :name "Launch Program"
                                       :runtimeArgs ["-r" "ts-node/register"]
                                       :args ["/home/edward/Projects/ts-scratch/index.ts"])))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package yasnippet)

(use-package all-the-icons
  ;; don't forget to M-x all-the-icons-install-fonts
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

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

(use-package mode-line-bell
  :init (mode-line-bell-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package mhtml-mode
          ;; so ace-window keybindings don't get overridden
  :config (define-key mhtml-mode-map (kbd "M-o") nil))

(use-package god-mode
  :init (god-mode)

  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (define-key god-local-mode-map (kbd "z") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (global-set-key (kbd "C-i") #'god-local-mode)
  (add-to-list 'god-exempt-major-modes 'vterm-mode))

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
(setq browse-url-generic-program (if (eq window-system 'ns) "open" "google-chrome-stable")
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

(setq create-lockfiles nil)

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

(defun ep/ts-eval ()
  "Compile the current file, then run it.
Outputs the results to a dedicated buffer."
  (interactive)
  (shell-command "node -r ts-node/register index.ts" "*ts-eval*")
  (with-current-buffer "*ts-eval*" (js-mode)
                       (setq-local flycheck-disabled-checkers '(javascript-eslint))))

;; (use-package nord-theme
;;   :after color
;;   :config
;;   (load-theme 'nord t))
;;   ;; (set-terminal-parameter nil 'background-mode 'dark)
;;   ;; (set-frame-parameter nil 'background-mode 'dark))



(global-set-key (kbd "C-c t e") 'ep/ts-eval)

(provide 'init)
;;; init.el ends here
