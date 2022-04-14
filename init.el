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

(defvar my-packages
  ;; Must `brew install mu fetchmail` for email support separately
  
  '(paredit
    exec-path-from-shell
    clojure-mode
    ;; extra syntax highlighting for clojure
    ;; clojure-mode-extra-font-locking
    smex
    projectile
    company
    counsel
    ivy
    ng2-mode
    znc
    bufler
    erc-hl-nicks
    json-mode
    load-dir
    org-mime
    php-mode
    ag
    exec-path-from-shell))

(load-theme 'solarized-dark t)

(dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package magit)
(use-package cider)
(use-package which-key
  :config (which-key-mode))
(use-package tt-mode
  :config
  (autoload 'tt-mode "tt-mode"))
(use-package solarized-theme  ;; https://github.com/bbatsov/solarized-emacs
  :config
  (set-terminal-parameter nil 'background-mode 'dark)
  (set-frame-parameter nil 'background-mode 'dark))
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (setq tide-format-options '(
			      :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
			      :placeOpenBraceOnNewLineForFunctions nil
			      :indentSize 2
			      :tabSize 2
			      :placeOpenBraceOnNewLineForFunctions nil
			      :placeOpenBraceOnNewLineForControlBlocks nil))
  (add-hook 'before-save-hook 'tide-format-before-save))



; used by mu4e to show images
(imagemagick-register-types)


(setq auto-mode-alist
      (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))

(add-to-list 'load-path "~/.emacs.d/customisations")
(load "theme.el")
(load "sexpers.el")
;(load "clipboard.el")
;;;(load "init-flycheck.el")
(use-package flycheck
  :init (global-flycheck-mode))

(load "init-shell.el")
(load "init-tide.el")
(load "init-erc.el")
(load "init-json.el")
;(load "init-mu4e.el")

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
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

(global-set-key (kbd "C-x C-b") 'bufler)

(projectile-mode +1)
(setq projectile-mode-line "Projectile")
; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec.ts")


(global-linum-mode)

(setq use-package-always-defer t
      use-package-always-ensure t)

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
   (setq sbt:program-options '("-Dsbt.supershell=false"))
   )

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "C-c l")
  
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         ;;(perl-mode . lsp)
         
  :config
  (setq lsp-enable-snippet nil) ;; try disabling for now as getting weird indentation problems
  (setq lsp-enable-indentation nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf))

  ;(lsp-register-client
  ;  (make-lsp-client :new-connection (lsp-tramp-connection '("perl" "MPerl::LanguageServer" "-e" "Perl::LanguageServer::run" "--"))
  ;                   :major-modes '(perl-mode)
  ;                   :remote? t
  ;                   :server-id 'perl-ls))

;; Performance
(setq gc-cons-threshold 100000000) ;; 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-metals)

(use-package yasnippet)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq backup-directory-alist
          `((".*" . ,(concat user-emacs-directory "backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups/") t)))

(setq inhibit-startup-screen t)
(blink-cursor-mode 0)

(setq ediff-split-window-function 'split-window-sensibly)
(setq ediff-ignore-similar-regions t)

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

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(windmove-default-keybindings)

(winner-mode)

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

(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))

(set-fontset-font t 'symbol 
                  (font-spec :family "Apple Color Emoji") 
                  nil 'prepend)

(add-hook 'prog-mode-hook
          (defun before-ng2-ts-mode()
            (when (eq major-mode 'ng2-ts-mode)
              (typescript-mode))))

;; Makes emoji work on MacOS
(set-fontset-font t '(#x1f000 . #x1faff) (font-spec :family "Apple Color Emoji"))

(require 'org-inlinetask)

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

(defalias 'perl-mode 'cperl-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(repeat-mode)

(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

(defun ep--search-phab ()
  "Search Phabricator for this filename."
  (interactive)
  (browse-url
   (format "https://vx-phabricator.wcn.co.uk/diffusion/VX/browse/master/?find=%s"
           (file-name-nondirectory (buffer-file-name)))))

(define-key global-map (kbd "C-c w p") 'ep--search-phab)

(defun ep--proc-done (process signal)
  "Show a notification that prints 'Done'.
PROCESS: The process to watch until done
SIGNAL: The signal the program exited with"
  (when (memq (process-status process) '(exit signal))
    (notifications-notify :title "'make' done")
    (shell-command-sentinel process signal)))

(defun ep--async-shell-command (command done-fn)
  "Run COMMAND and when finished run DONE-FN."
  (let* ((output-buffer (generate-new-buffer "*Async shell command*"))
       (proc (progn
               (async-shell-command command output-buffer)
               (get-buffer-process output-buffer))))
  (if (process-live-p proc)
      (set-process-sentinel proc done-fn)
    (message "No process running!"))))

(defun ep--make-ui ()
  "Run the command to build the UI."
  (interactive)
  ;;(async-shell-command "make -C ~/dev/vX/WCN/ui all")
  (ep--async-shell-command "make -C ~/dev/vX/WCN/ui all" #'ep--proc-done))

(define-key global-map (kbd "C-c w m") 'ep--make-ui)

(defun ep--restart-apache ()
  "Run the command to restart apache."
  (interactive)
  (ep--async-shell-command "sudo /etc/init.d/apache2 restart" #'ep--proc-done))

(define-key global-map (kbd "C-c w r") 'ep--restart-apache)


(provide 'init)
;;; init.el ends here

