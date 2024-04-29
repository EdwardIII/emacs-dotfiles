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

;; ;; TODO: Do this automatically
(setenv "PERL_MB_OPT" "--install_base \"/home/edward/perl5\""); export PERL_MB_OPT;
(setenv "PERL_MM_OPT" "INSTALL_BASE=/home/edward/perl5"); export PERL_MM_OPT;

(use-package nord-theme
  :config
  (defun init-nord-theme (frame)
    "Initialise the nord theme. Unloads itself after being used once."
    (with-selected-frame frame (load-theme 'nord t))
    (remove-hook 'after-make-frame-functions #'init-nord-theme))

  ;; so we still get the theme in not-daemon-mode
  (when (frame-list) (load-theme 'nord t))
  (add-hook 'after-make-frame-functions #'init-nord-theme)) ; in daemon mode the theme is started again after the gui first opens

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

(use-package mhtml-mode)
(use-package scss-mode)
(use-package php-mode)
(use-package magit)

(use-package js)

(use-package elisp-format
  :init (require 'elisp-format))
;; (use-package cider
;;   :pin melpa-stable)
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
;; (use-package ivy
;;   :bind (("C-'" . 'avy-goto-char-2))
;;   :init
;;   (ivy-mode)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (global-set-key (kbd "C-s") 'swiper-isearch))
(use-package flycheck
  :init
  (global-flycheck-mode)
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
    :modes (scss-mode))

  (defhydra hydra-flycheck (global-map "C-c C-c")
    "flycheck errors"
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" lsp-execute-code-action "typescript fix")
    ("RET" nil "cancel")))

;; ;; If you want fuzzy matching, check out:
;; ;; https://gitlab.com/com-informatimago/emacs/-/blob/master/pjb-clelp.el
;; ;; https://gitlab.com/com-informatimago/emacs/-/blob/master/pjb-sources.el#L3538
;; ;; Or maybe vertico + orderless?
;; (use-package counsel
;;   :init
;;   (counsel-mode)
;;   :bind (("M-x" . 'counsel-M-x)
;;          ( "C-c k" . 'counsel-ag)))
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-allow-imprecise-window-fit nil))
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

(use-package typescript-ts-mode
  :mode ("\\.ts" . typescript-ts-mode)
  :bind (("C-c t f" . lsp-execute-code-action)))

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "C-c l")

  :hook
  (typescript-ts-mode . lsp)
  (typescript-mode . lsp)
  (ruby-mode . lsp)
  (scss-mode . lsp) ;; don't forget to M-x lsp-install-server RET css-ls RET
  (json-mode . lsp)
  ;; for angular html integration don't forget to select the right version number and run: npm install -g @angular/language-service@next typescript @angular/language-server@v12.2.3
  (mhtml-mode . lsp)
  ;; (cperl-mode . lsp)
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

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection
  ;;                                    (lambda ()
  ;;                                      (list lsp-perl-language-server-path
  ;;                                            "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"
  ;;                                                                                                                                      "-Ilib"
  ;;                                            "--"
  ;;                                                                                         "-Ilib"

  ;;                                            (format "--port %d --version %s"
  ;;                                                    lsp-perl-language-server-port lsp-perl-language-server-client-version))))
  ;;                   :major-modes '(perl-mode cperl-mode)
  ;;                   :initialized-fn (lambda (workspace)
  ;;                                     (with-lsp-workspace workspace
  ;;                                       (lsp--set-configuration
  ;;                                        (lsp-configuration-section "perl"))))
  ;;                   :priority 2
  ;;                   :environment-fn (lambda () '(("PERL_MB_OPT" . "--install_base \"/home/edward/perl5\"")
  ;;                                                ("PERL_MM_OPT" . "\"INSTALL_BASE=/home/edward/perl5\"")
  ;;                                                ("PERL5LIB" . "/home/edward/perl5/lib/perl5:./lib:./WCN/lib")))
  ;;                   :server-id 'perl-language-server-local-lib))
  )

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
  :preface
  (defun ep-god-mode-off () (interactive) (god-mode-activate -1) (newline nil 'interactive))
  (defun ep-god-mode-on () (interactive) (god-mode-activate t))
  (defun ep/push-mark-silent () (interactive) (push-mark (point) t nil))
  :hook (god-local-mode . ep/push-mark-silent)
  :after (copilot ; so we can stomp copilot's tab keybinding in god mode
          flycheck
          lsp-mode)
  :init (god-mode)
  :bind (
         ("RET" . ep-god-mode-off)
         ("C-i" . #'god-local-mode)
         :map god-local-mode-map
         ("TAB" . indent-for-tab-command)
         ("i" . #'god-local-mode)
         ("z" . #'repeat))

  :config
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (add-to-list 'god-exempt-major-modes 'vterm-mode 'circe-channel-mode)
  (add-hook 'before-save-hook #'ep-god-mode-on))

(use-package button-lock
  :preface (defun setup-buttonlock ()
             (button-lock-mode)
             (button-lock-set-button "\\<https://[^[:space:]\n]+" 'browse-url-at-mouse
                                     :face 'link
                                     :face-policy 'prepend))
  :hook ((vterm-mode . setup-buttonlock)
         (magit-process-mode . setup-buttonlock)
         (comint-mode . setup-buttonlock)
         (arc-mode . setup-buttonlock)))

(use-package flyspell
  ;; need to make it more specific, text-mode applies to too many modes like HTML+
  ;; :hook ((text-mode . flyspell-mode))
  )

(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  :hook ((js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path (lambda () (expand-file-name "vendor/combobulate/" user-emacs-directory)))

;; All required by copilot.el
(use-package dash)
(use-package s)
(add-to-list 'load-path (expand-file-name "vendor/copilot/" user-emacs-directory))
(use-package copilot
  :ensure nil
  :requires (dash s)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("TAB"   . copilot-accept-completion)))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package better-jumper
  :init (better-jumper-mode +1))


(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . isearch-forward)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; next ideas? vertico, consult, orderless, embark and marginalia

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :init (setq deft-directory "~/notes"
                deft-extensions '("md" "org" "txt")))

(use-package inf-clojure
  :hook (clojure-mode . inf-clojure-minor-mode))

(load "sexpers.el")
(load "init-shell.el")
;; (load "init-tide.el")
(load "init-irc.el")

(set-face-attribute 'default nil :height 140)

(global-set-key (kbd "C-<return>") 'set-mark-command)
(global-set-key (kbd "C-o") 'pop-global-mark)


(global-display-line-numbers-mode)
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

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key(kbd "C-M-r") 'isearch-backward)

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

(setq create-lockfiles nil)

(setq max-mini-window-height 10)

(require 'ffap)
(defun browse-last-url-in-brower ()
  "Find the last url in the buffer."
  (interactive)
  (save-excursion
    (ffap-next-url t t)))

;; TODO: eh?
(global-set-key (kbd "C-c u") #'ffap-next-url)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
(require 'treesit)

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
(global-set-key (kbd "C-c t e") 'ep/ts-eval)

(defun ep/send-to-bottom ()
  "Send the current buffer to a bottom sidebar."
  (display-buffer-in-side-window (current-buffer) '((side . bottom))))

(defun ep/clear-read-only-buffer ()
  "Clear the current buffer, even if it is read-only."
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

(require 'tramp)
(define-derived-mode arc-mode nil
  "Arc"
  "Mode for arcanist output.")
(declare-function ansi-color-apply-on-region "ansi-color")
(defun ep/arc-diff ()
  "Create an arc diff against master.
Message is set to any unique commits on this branch."
  (interactive)
  ;; (setq-local temporary-file-directory (tramp-handle-temporary-file-directory))
  (let ((tempfile (tramp-handle-make-nearby-temp-file "aggregate-commits" nil ".txt")))
    (write-region  (ep/aggregate-commits)
                   nil
                   tempfile)
    (shell-command
     (format "%s \"%s\" %s" "arc diff --message-file " (file-remote-p tempfile 'localname) "master")
     "*arc-diff*"
     )
    (with-current-buffer "*arc-diff*"
      (switch-to-buffer "*arc-diff*")
      (ansi-color-apply-on-region (point-min) (point-max))
      (setq buffer-read-only t)
      (arc-mode))))

(defun ep/aggregate-commits ()
  "Return titles & messages for all commits on this branch."
  (interactive)
  (shell-command-to-string "git log --reverse --format='%s%n%n%b' ^master HEAD"))

(defun ep/frame= (frame-name frame)
  "Does the FRAME have the given FRAME-NAME?"
  (unless (framep frame) (error "FRAME must be a frame object"))
  (string-equal (frame-parameter frame 'name) frame-name))

(defun ep/frame-by-name (name)
  "Get frame by NAME, string.  nil if not found."
  (car
   (seq-filter
    (lambda (x) (ep/frame= name x))
    (frame-list))))

;;;###autoload
(define-derived-mode lbl-ts-mode prog-mode "Lbl"
  (when (treesit-ready-p 'lbl)
    (treesit-parser-create 'lbl)
    (setq-local treesit-font-lock-settings
                (treesit-font-lock-rules
                 :language 'lbl
                 :feature 'keyword
                 '((label) @font-lock-keyword-face)

                 :language 'lbl
                 :feature 'prefix
                 :override t
                 '((prefix (text)) @font-lock-comment-face)

                 :language 'lbl
                 :feature 'error
                 :override t
                 '((ERROR) @next-error)))
  (setq-local treesit-font-lock-feature-list '((prefix) (keyword) (error)))
  (treesit-major-mode-setup)))

(if (treesit-ready-p 'typescript)
    (add-to-list 'auto-mode-alist '("\\.lbl\\'" . lbl-ts-mode)))

(defun ep/copy-full-path-to-kill-ring ()
  "Copy the full path of the current buffer's file to the kill ring."
  (interactive)
  (if (buffer-file-name)
      (progn
        (kill-new (file-truename (buffer-file-name)))
        (message "Copied buffer's full path to kill ring: %s" (buffer-file-name)))
    (message "Current buffer is not visiting a file!")))

(provide 'init)
;;; init.el ends here
